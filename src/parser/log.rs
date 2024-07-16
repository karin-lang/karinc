use crate::log::{CompilerErr, CompilerLog, SyntaxErrorKind};
use crate::parser::*;

pub type ParserResult<T> = Result<T, ParserLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {
    DuplicateItemName { id: Id, span: Span },
    ExpectedActualArg { span: Span },
    ExpectedExpr { span: Span },
    ExpectedFormalArg { span: Span },
    ExpectedId { span: Span },
    ExpectedItem { span: Span },
    ExpectedKeyword { keyword: Keyword, span: Span },
    ExpectedToken { kind: TokenKind, span: Span },
    ExpectedType { span: Span },
    UnexpectedEof { span: Span },
    UnknownMarkerName { span: Span },
}

impl From<ParserLog> for CompilerLog {
    fn from(value: ParserLog) -> Self {
        match value {
            ParserLog::DuplicateItemName { id, span } => CompilerLog::err(CompilerErr::DuplicateItemName { id }, span),
            ParserLog::ExpectedActualArg { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedActualArg, span),
            ParserLog::ExpectedExpr { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedExpr, span),
            ParserLog::ExpectedFormalArg { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedFormalArg, span),
            ParserLog::ExpectedId { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedId, span),
            ParserLog::ExpectedItem { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedItem, span),
            ParserLog::ExpectedKeyword { keyword, span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedKeyword { keyword }, span),
            ParserLog::ExpectedToken { kind, span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedToken { kind }, span),
            ParserLog::ExpectedType { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedType, span),
            ParserLog::UnexpectedEof { span } => CompilerLog::syntax_err(SyntaxErrorKind::UnexpectedEof, span),
            ParserLog::UnknownMarkerName { span } => CompilerLog::syntax_err(SyntaxErrorKind::UnknownMarkerName, span),
        }
    }
}
