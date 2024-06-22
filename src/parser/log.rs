use crate::log::{CompilerLog, SyntaxErrorKind};
use crate::parser::*;

pub type ParserResult<T> = Result<T, ParserLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {
    ExpectedExpr { span: Span },
    ExpectedId { span: Span },
    ExpectedItem { span: Span },
    ExpectedFormalArg { span: Span },
    ExpectedActualArg { span: Span },
    ExpectedKeyword { keyword: Keyword, span: Span },
    ExpectedToken { kind: TokenKind, span: Span },
    ExpectedType { span: Span },
    UnexpectedEof { span: Span },
}

impl From<ParserLog> for CompilerLog {
    fn from(value: ParserLog) -> Self {
        match value {
            ParserLog::ExpectedExpr { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedExpr, span),
            _ => unimplemented!(),
        }
    }
}
