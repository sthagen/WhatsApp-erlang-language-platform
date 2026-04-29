/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unexpected separator (W0018)
//
// Diagnostic for unexpected separators in function clauses.

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::ast::ClauseSeparator;

use super::DiagnosticCode;
use super::GenericLinter;
use super::GenericLinterMatchContext;
use super::Linter;
use super::LinterContext;

pub(crate) struct UnexpectedSeparatorLinter;

impl Linter for UnexpectedSeparatorLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnexpectedSeparator
    }

    fn description(&self) -> &'static str {
        "Unexpected separator in function clauses."
    }

    fn can_be_suppressed(&self) -> bool {
        false
    }
}

impl GenericLinter for UnexpectedSeparatorLinter {
    type Context = &'static str;

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let mut res = Vec::new();
        let file_id = ctx.file_id;
        let def_map = ctx.sema.def_map(file_id);

        def_map.get_functions().for_each(|(_, fun_def)| {
            let n = fun_def.function_clauses.len();
            fun_def
                .function_clauses
                .iter()
                .enumerate()
                .for_each(|(i, fun)| {
                    if i + 1 != n {
                        // Non-last clause: '.' is unexpected
                        if let Some((ClauseSeparator::Dot, range)) = fun.separator {
                            res.push(GenericLinterMatchContext {
                                range: FileRange { file_id, range },
                                context: "Unexpected '.'",
                            });
                        }
                    } else {
                        // Last clause: ';' is unexpected
                        if let Some((ClauseSeparator::Semi, range)) = fun.separator {
                            res.push(GenericLinterMatchContext {
                                range: FileRange { file_id, range },
                                context: "Unexpected ';'",
                            });
                        }
                    }
                })
        });

        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Borrowed(context)
    }
}

pub(crate) static LINTER: UnexpectedSeparatorLinter = UnexpectedSeparatorLinter;

#[cfg(test)]
mod tests {
    use crate::tests::check_diagnostics;

    #[test]
    fn fun_decl_misplaced_dot() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2;
   foo(2)->3.
         %% ^ warning: W0018: Unexpected '.'
   foo(3)->4.
"#,
        );
    }

    #[test]
    fn fun_decl_misplaced_semi() {
        check_diagnostics(
            r#"
   -module(main).
   foo(1)->2;
   foo(2)->3;
         %% ^ warning: W0018: Unexpected ';'
"#,
        );
    }
}
