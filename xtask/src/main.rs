/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This provides extra build system commands, most notably:
//! `cargo xtask codegen` for code generation.

use std::env;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use anyhow::bail;
use bpaf::Bpaf;
use bpaf::Parser;
use bpaf::construct;
use xshell::Shell;
use xshell::cmd;

use crate::Command::Help;

mod codegen;

fn main() -> Result<()> {
    let args = args().run();
    match args.command {
        Command::CodeGen(_) => {
            let mode = codegen::Mode::Overwrite;
            codegen::CodegenCmd { mode }.run()
        }
        Help() => {
            eprintln!(
                "\
cargo xtask
Run custom build command.

USAGE:
    cargo xtask <SUBCOMMAND>

SUBCOMMANDS:
    codegen"
            );
            Ok(())
        }
    }
}

#[derive(Debug, Clone, Bpaf)]
#[bpaf(options)]
struct Args {
    #[bpaf(external(command))]
    command: Command,
}

#[derive(Clone, Debug)]
enum Command {
    CodeGen(CodeGen),
    Help(),
}

fn command() -> impl Parser<Command> {
    let code_gen = code_gen()
        .map(Command::CodeGen)
        .to_options()
        .command("codegen")
        .help("Generate ast from tree-sitter grammar");

    construct!([code_gen]).fallback(Help())
}

#[derive(Clone, Debug, Bpaf)]
struct CodeGen {}

pub fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

const PREAMBLE: &str = "\x40generated file, do not edit by hand, see `xtask/src/codegen.rs`";

pub fn reformat(text: &str) -> Result<String> {
    let sh = Shell::new()?;
    ensure_rustfmt(&sh)?;
    let rustfmt_toml = project_root().join("rustfmt.toml");
    let stdout = cmd!(sh, "rustfmt --config-path {rustfmt_toml} ")
        .stdin(text)
        .read()?;
    Ok(format!("//! {PREAMBLE}\n\n{stdout}\n"))
}

fn ensure_rustfmt(sh: &Shell) -> Result<()> {
    let out = cmd!(sh, "rustfmt --version").read()?;
    if !out.contains("stable") {
        bail!(
            "Failed to run rustfmt from toolchain 'stable'. \
             Please run `rustup component add rustfmt --toolchain stable` to install it.",
        )
    }
    Ok(())
}
