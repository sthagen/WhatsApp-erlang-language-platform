/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Preprocessor state machine for conditional compilation.

use std::collections::BTreeSet;
use std::sync::Arc;

use fxhash::FxHashMap;

use crate::DefineId;
use crate::InFile;
use crate::MacroName;
use crate::Name;
use crate::PPConditionId;
use crate::form_list::PPConditionResult;

// ============================================================================
// Macro environment for preprocessing
// ============================================================================

/// The set of macros predefined for preprocessing.
///
/// This struct holds the predefined macro names and the module name context
/// that are used when evaluating preprocessor conditions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct MacroEnvironment {
    pub predefined: BTreeSet<MacroName>,
    pub module_name: Option<Name>,
    /// When true, enables new ifdef/ifndef condition evaluation (experimental).
    /// When false, all forms are treated as active (legacy behavior).
    /// Default: false (disabled - matches legacy behavior)
    pub new_ifdef: bool,
}

impl MacroEnvironment {
    /// Create a new empty macro environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Define a macro in this environment.
    pub fn define(&mut self, name: MacroName) {
        self.predefined.insert(name);
    }

    /// Set the module name for this environment.
    pub fn set_module_name(&mut self, name: Name) {
        self.module_name = Some(name);
    }

    /// Get the module name for this environment.
    pub fn module_name(&self) -> Option<&Name> {
        self.module_name.as_ref()
    }

    /// Set whether new ifdef/ifndef condition evaluation is enabled.
    pub fn set_new_ifdef(&mut self, value: bool) {
        self.new_ifdef = value;
    }

    /// Create an environment with common test macros defined.
    ///
    /// This is useful for testing preprocessor behavior where
    /// certain macros are expected to be predefined.
    #[cfg(test)]
    pub fn with_test_macros() -> Self {
        let mut env = Self::new();
        env.define(MacroName::new(Name::from_erlang_service("TEST"), None));
        env.define(MacroName::new(Name::from_erlang_service("DEBUG"), None));
        env
    }
}

/// State during sequential file processing.
#[derive(Debug, Clone)]
pub struct PreprocessorState {
    condition_stack: Vec<ConditionFrame>,
    defined_macros: BTreeSet<MacroName>,
    macro_definitions: FxHashMap<MacroName, InFile<DefineId>>,
    is_active: bool,
    module_name: Option<Name>,
}

#[derive(Debug, Clone)]
struct ConditionFrame {
    has_taken_branch: bool,
    parent_active: bool,
    is_unknown: bool,
}

/// Snapshot of preprocessor state at a point in time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocessorSnapshot {
    pub defined: Arc<BTreeSet<MacroName>>,
    pub is_active: bool,
    pub is_unknown: bool,
}

/// Result of preprocessor analysis for a file.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PreprocessorAnalysis {
    condition_results: FxHashMap<PPConditionId, PPConditionResult>,
    pub final_state: Option<PreprocessorSnapshot>,
}

impl PreprocessorState {
    pub fn new(env: &MacroEnvironment) -> Self {
        Self {
            condition_stack: Vec::new(),
            defined_macros: env.predefined.clone(),
            macro_definitions: FxHashMap::default(),
            is_active: true,
            module_name: env.module_name.clone(),
        }
    }

    pub fn is_active(&self) -> bool {
        self.is_active
    }

    pub fn is_defined(&self, name: &MacroName) -> bool {
        self.defined_macros.contains(name)
    }

    pub fn defined_macros(&self) -> &BTreeSet<MacroName> {
        &self.defined_macros
    }

    pub fn enter_ifdef(&mut self, name: &MacroName) {
        let condition_met = self.is_defined(name);
        self.push_condition(condition_met);
    }

    pub fn enter_ifndef(&mut self, name: &MacroName) {
        let condition_met = !self.is_defined(name);
        self.push_condition(condition_met);
    }

    pub fn enter_else(&mut self) {
        if let Some(frame) = self.condition_stack.last_mut() {
            if frame.is_unknown {
                // If the chain is unknown, else is also unknown
                self.is_active = false;
                // Keep the frame marked as unknown
            } else {
                let should_take = frame.parent_active && !frame.has_taken_branch;
                self.is_active = should_take;
                if should_take {
                    frame.has_taken_branch = true;
                }
            }
        }
    }

    pub fn exit_condition(&mut self) {
        if let Some(frame) = self.condition_stack.pop() {
            self.is_active = frame.parent_active;
        }
    }

    pub fn define_macro(&mut self, name: MacroName, definition: Option<InFile<DefineId>>) {
        self.defined_macros.insert(name.clone());
        if let Some(def) = definition {
            self.macro_definitions.insert(name, def);
        }
    }

    pub fn undefine_macro(&mut self, name: &MacroName) {
        self.defined_macros.remove(name);
        self.macro_definitions.remove(name);
    }

    pub fn module_name(&self) -> Option<&Name> {
        self.module_name.as_ref()
    }

    pub fn snapshot(&self) -> PreprocessorSnapshot {
        PreprocessorSnapshot {
            defined: Arc::new(self.defined_macros.clone()),
            is_active: self.is_active,
            is_unknown: self.is_current_unknown(),
        }
    }

    pub fn merge_from_include(&mut self, other_macros: &BTreeSet<MacroName>) {
        self.defined_macros.extend(other_macros.iter().cloned());
    }

    fn push_condition(&mut self, condition_met: bool) {
        let parent_active = self.is_active;
        let should_take = parent_active && condition_met;

        self.condition_stack.push(ConditionFrame {
            has_taken_branch: should_take,
            parent_active,
            is_unknown: false,
        });

        self.is_active = should_take;
    }

    #[allow(dead_code)] // Used in later commits for condition evaluation
    pub(crate) fn push_unknown_condition(&mut self) {
        let parent_active = self.is_active;

        self.condition_stack.push(ConditionFrame {
            has_taken_branch: false,
            parent_active,
            is_unknown: true,
        });

        // Unknown conditions are treated as inactive for form activity purposes,
        // but we track that they're unknown so we can report the correct result
        self.is_active = false;
    }

    /// Check if the current condition frame (if any) has an unknown result
    pub fn is_current_unknown(&self) -> bool {
        self.condition_stack
            .last()
            .map(|f| f.is_unknown)
            .unwrap_or(false)
    }
}

impl PreprocessorAnalysis {
    pub fn new() -> Self {
        Self::default()
    }

    /// Query the result of a specific preprocessor condition.
    /// Returns `Active` if the condition is not found (default behavior).
    pub fn is_condition_active(&self, cond_id: PPConditionId) -> PPConditionResult {
        self.condition_results
            .get(&cond_id)
            .copied()
            .unwrap_or(PPConditionResult::Active)
    }
}

#[cfg(test)]
mod tests {
    use crate::MacroName;
    use crate::Name;
    use crate::preprocessor::MacroEnvironment;
    use crate::preprocessor::PreprocessorState;

    fn name(s: &str) -> Name {
        Name::from_erlang_service(s)
    }

    fn macro_name(s: &str) -> MacroName {
        MacroName::new(name(s), None)
    }

    // ========================================================================
    // MacroEnvironment tests
    // ========================================================================

    #[test]
    fn test_macro_environment_new() {
        let env = MacroEnvironment::new();
        assert!(env.predefined.is_empty());
        assert!(env.module_name.is_none());
    }

    #[test]
    fn test_macro_environment_define() {
        let mut env = MacroEnvironment::new();
        let foo = macro_name("FOO");
        let bar = macro_name("BAR");

        env.define(foo.clone());
        assert!(env.predefined.contains(&foo));
        assert!(!env.predefined.contains(&bar));

        env.define(bar.clone());
        assert!(env.predefined.contains(&foo));
        assert!(env.predefined.contains(&bar));
    }

    #[test]
    fn test_macro_environment_module_name() {
        let mut env = MacroEnvironment::new();
        assert!(env.module_name().is_none());

        env.set_module_name(name("my_module"));
        assert_eq!(env.module_name(), Some(&name("my_module")));
    }

    // ========================================================================
    // PreprocessorState tests
    // ========================================================================

    #[test]
    fn test_preprocessor_state_new() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("TEST"));
        env.set_module_name(name("my_module"));

        let state = PreprocessorState::new(&env);

        assert!(state.is_active());
        assert!(state.is_defined(&macro_name("TEST")));
        assert_eq!(state.module_name(), Some(&name("my_module")));
    }

    #[test]
    fn test_preprocessor_state_ifdef_defined() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("TEST"));
        let mut state = PreprocessorState::new(&env);

        state.enter_ifdef(&macro_name("TEST"));
        // Should be active because TEST is defined
        assert!(state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_ifdef_undefined() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        state.enter_ifdef(&macro_name("TEST"));
        // Should be inactive because TEST is not defined
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_ifndef_defined() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("TEST"));
        let mut state = PreprocessorState::new(&env);

        state.enter_ifndef(&macro_name("TEST"));
        // Should be inactive because TEST IS defined
        assert!(!state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_ifndef_undefined() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        state.enter_ifndef(&macro_name("TEST"));
        // Should be active because TEST is not defined
        assert!(state.is_active());

        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_nested_conditions() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("OUTER"));
        let mut state = PreprocessorState::new(&env);

        // ifdef OUTER (defined) -> active
        state.enter_ifdef(&macro_name("OUTER"));
        assert!(state.is_active());

        // ifdef INNER (not defined) -> inactive
        state.enter_ifdef(&macro_name("INNER"));
        assert!(!state.is_active());

        // exit inner
        state.exit_condition();
        assert!(state.is_active());

        // exit outer
        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_nested_conditions_parent_inactive() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        // ifdef OUTER (not defined) -> inactive
        state.enter_ifdef(&macro_name("OUTER"));
        assert!(!state.is_active());

        // ifdef INNER -> should remain inactive regardless
        // (even if INNER were defined, parent is inactive)
        state.enter_ifdef(&macro_name("INNER"));
        assert!(!state.is_active());

        // exit inner
        state.exit_condition();
        assert!(!state.is_active());

        // exit outer
        state.exit_condition();
        assert!(state.is_active());
    }

    #[test]
    fn test_preprocessor_state_define_macro() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        assert!(!state.is_defined(&macro_name("NEW_MACRO")));

        state.define_macro(macro_name("NEW_MACRO"), None);

        assert!(state.is_defined(&macro_name("NEW_MACRO")));
    }

    #[test]
    fn test_preprocessor_state_undefine_macro() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("EXISTING"));
        let mut state = PreprocessorState::new(&env);

        assert!(state.is_defined(&macro_name("EXISTING")));

        state.undefine_macro(&macro_name("EXISTING"));

        assert!(!state.is_defined(&macro_name("EXISTING")));
    }

    #[test]
    fn test_preprocessor_state_snapshot() {
        let mut env = MacroEnvironment::new();
        env.define(macro_name("FOO"));
        let mut state = PreprocessorState::new(&env);

        // Take snapshot when active
        let snapshot1 = state.snapshot();
        assert!(snapshot1.is_active);
        assert!(snapshot1.defined.contains(&macro_name("FOO")));

        // Make inactive
        state.enter_ifdef(&macro_name("UNDEFINED"));
        let snapshot2 = state.snapshot();
        assert!(!snapshot2.is_active);
    }

    #[test]
    fn test_preprocessor_state_merge_from_include() {
        let env = MacroEnvironment::new();
        let mut state = PreprocessorState::new(&env);

        let mut include_macros = std::collections::BTreeSet::new();
        include_macros.insert(macro_name("INCLUDED_MACRO"));

        state.merge_from_include(&include_macros);

        assert!(state.is_defined(&macro_name("INCLUDED_MACRO")));
    }
}
