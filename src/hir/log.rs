use crate::log::{CompilerErr, CompilerLog, SyntaxErrorKind};
use crate::lexer::token::Span;
use crate::parser::ast::{Id, Path};
use crate::hir::id::*;

pub type HirLoweringResult<T> = Result<T, HirLoweringLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    ExpectedExprButFoundHako { hako_id: HakoId, span: Span },
    ExpectedExprButFoundMod { mod_id: ModId, span: Span },
    GlobalIdIsNotFound { global_id: GlobalId, span: Span },
    IdIsNotFoundInScope { id: Id, span: Span },
    PathIsNotFoundInScope { path: Path, span: Span },
    UnnecessaryPath { path: Path, span: Span },
}

impl From<HirLoweringLog> for CompilerLog {
    fn from(value: HirLoweringLog) -> Self {
        match value {
            HirLoweringLog::ExpectedExprButFoundHako { hako_id, span } => CompilerLog::syntax_err(
                SyntaxErrorKind::ExpectedExprButFoundHako { hako_id },
                span,
            ),
            HirLoweringLog::ExpectedExprButFoundMod { mod_id, span } => CompilerLog::syntax_err(
                SyntaxErrorKind::ExpectedExprButFoundMod { mod_id },
                span,
            ),
            HirLoweringLog::GlobalIdIsNotFound { global_id, span } => CompilerLog::err(
                CompilerErr::GlobalIdIsNotFound { global_id },
                span,
            ),
            HirLoweringLog::IdIsNotFoundInScope { id, span } => CompilerLog::err(
                CompilerErr::IdIsNotFoundInScope { id },
                span,
            ),
            HirLoweringLog::PathIsNotFoundInScope { path, span } => CompilerLog::err(
                CompilerErr::PathIsNotFoundInScope { path },
                span,
            ),
            HirLoweringLog::UnnecessaryPath { path, span } => CompilerLog::err(
                CompilerErr::UnnecessaryPath { path },
                span,
            ),
        }
    }
}
