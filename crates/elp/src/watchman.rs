/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide::elp_ide_db::elp_base_db::SourceRoot;
use elp_ide::elp_ide_db::elp_base_db::SourceRootId;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use paths::Utf8PathBuf;
use serde::Deserialize;

use crate::build::types::LoadResult;
use crate::document::Document;

#[derive(Debug, Clone, Deserialize)]
pub struct Watchman {
    pub watch: PathBuf,
}

#[derive(Debug, Clone, Deserialize)]
pub struct WatchmanClock {
    pub clock: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct WatchmanChanges {
    pub files: Vec<WatchmanFile>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct WatchmanFile {
    pub name: String,
    pub exists: bool,
    #[serde(default)]
    pub new: bool,
}

impl Watchman {
    pub fn cmd() -> Command {
        Command::new("watchman")
    }

    pub fn new(project: &Path) -> Result<Self> {
        let mut cmd = Self::cmd();
        cmd.arg("watch-project");
        cmd.arg(project.as_os_str());
        Ok(serde_json::from_slice(&cmd.output()?.stdout)?)
    }

    pub fn get_clock(&self) -> Result<WatchmanClock> {
        let mut cmd = Self::cmd();
        cmd.arg("clock");
        cmd.arg(self.watch.as_os_str());
        Ok(serde_json::from_slice(&cmd.output()?.stdout)?)
    }

    pub fn get_changes(
        &self,
        from: &WatchmanClock,
        patterns: Vec<&str>,
    ) -> Result<WatchmanChanges> {
        let mut cmd = Self::cmd();
        cmd.arg("since");
        cmd.arg(self.watch.as_os_str());
        cmd.arg(&from.clock);
        cmd.args(patterns);
        Ok(serde_json::from_slice(&cmd.output()?.stdout)?)
    }
}

// Adapted from elp::server
pub fn process_changes_to_vfs_store(loaded: &mut LoadResult) -> bool {
    let changed_files = loaded.vfs.take_changes();

    if changed_files.is_empty() {
        return false;
    }

    let raw_database = loaded.analysis_host.raw_database_mut();

    for (_, file) in &changed_files {
        let file_exists = loaded.vfs.exists(file.file_id);
        if file.change != vfs::Change::Delete && file_exists {
            if let vfs::Change::Create(v, _) | vfs::Change::Modify(v, _) = &file.change {
                let document = Document::from_bytes(v);
                let (text, line_ending) = document.vfs_to_salsa();
                raw_database.set_file_text(file.file_id, Arc::from(text));
                loaded.line_ending_map.insert(file.file_id, line_ending);
            } else {
                raw_database.set_file_text(file.file_id, Arc::from(""));
            };
        }
    }

    if changed_files
        .into_values()
        .any(|file| file.is_created_or_deleted())
    {
        let sets = loaded.file_set_config.partition(&loaded.vfs);
        for (idx, set) in sets.into_iter().enumerate() {
            let root_id = SourceRootId(idx as u32);
            for file_id in set.iter() {
                raw_database.set_file_source_root(file_id, root_id);
            }
            let root = SourceRoot::new(set);
            raw_database.set_source_root(root_id, Arc::new(root));
        }
    }

    true
}

pub fn should_reload_project(watchman: &Watchman, last_read: &WatchmanClock) -> Result<bool> {
    let project_paths = vec![
        "BUCK",
        "TARGETS",
        "TARGETS.v2",
        "rebar.config",
        "rebar.config.script",
        "**/BUCK",
        "**/TARGETS",
        "**/TARGETS.v2",
        "**/rebar.config",
        "**/rebar.config.script",
    ];
    let project_path_changed = !watchman
        .get_changes(last_read, project_paths)?
        .files
        .is_empty();
    let suite_files = vec!["**/*_SUITE.erl"];
    let suite_file_created = watchman
        .get_changes(last_read, suite_files)?
        .files
        .iter()
        .any(|f| f.new || !f.exists);
    Ok(project_path_changed || suite_file_created)
}

pub fn update_changes(
    loaded: &mut LoadResult,
    watchman: &Watchman,
    last_read: &WatchmanClock,
) -> Result<WatchmanClock> {
    let vfs = &mut loaded.vfs;
    let time = watchman.get_clock()?;
    let file_changes = watchman.get_changes(last_read, vec!["**/*.hrl", "**/*.erl"])?;
    file_changes.files.into_iter().for_each(|file| {
        let path = watchman.watch.join(file.name);
        let vfs_path = VfsPath::from(AbsPathBuf::assert(
            Utf8PathBuf::from_path_buf(path.clone()).expect("UTF8 conversion failed"),
        ));
        if !file.exists {
            vfs.set_file_contents(vfs_path, None);
        } else {
            let contents =
                fs::read(&path).unwrap_or_else(|_| panic!("Cannot read created file {path:?}"));
            vfs.set_file_contents(vfs_path, Some(contents));
        }
    });
    process_changes_to_vfs_store(loaded);
    Ok(time)
}
