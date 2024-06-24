use crate::log::{CompilerErr, CompilerLog, SyntaxErrorKind};
use crate::lexer::token;
use crate::parser::ast;
use crate::hir::id::*;

pub type HirLoweringResult<T> = Result<T, HirLoweringLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    ExpectedExprButFoundHako { hako_id: HakoId, span: token::Span },
    ExpectedExprButFoundMod { mod_id: ModId, span: token::Span },
    GlobalIdIsNotFound { global_id: GlobalId, span: token::Span },
    PathIsNotFoundInScope { path: ast::Path, span: token::Span },
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
            HirLoweringLog::PathIsNotFoundInScope { path, span } => CompilerLog::err(
                CompilerErr::PathIsNotFoundInScope { path },
                span,
            ),
        }
    }
}
