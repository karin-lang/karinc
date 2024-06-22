use crate::log::{CompilerLog, SyntaxErrorKind};
use crate::lexer::token;
use crate::hir::id::*;

pub type HirLoweringResult<T> = Result<T, HirLoweringLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    ExpectedExprButFoundHako { hako_id: HakoId, span: token::Span },
    ExpectedExprButFoundMod { mod_id: ModId, span: token::Span },
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
        }
    }
}
