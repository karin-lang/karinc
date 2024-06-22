use crate::lexer::token::Span;
use crate::log::{CompilerLog, SyntaxErrorKind};

pub type LexerResult<T> = Result<T, LexerLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {
    EmptyCharLiteral { span: Span },
    ExpectedTypeSuffix { span: Span },
    LineBreakInCharLiteral { span: Span },
    LineBreakInStrLiteral { span: Span },
    TooLongCharLiteral { span: Span },
    UnclosedCharLiteral { span: Span },
    UnclosedStrLiteral { span: Span },
    UnknownEscseq { span: Span },
}

impl From<LexerLog> for CompilerLog {
    fn from(value: LexerLog) -> Self {
        match value {
            LexerLog::EmptyCharLiteral { span } => CompilerLog::syntax_err(SyntaxErrorKind::EmptyCharLiteral, span),
            _ => unimplemented!(),
        }
    }
}
