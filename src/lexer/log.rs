use crate::lexer::token::Span;
use crate::log::{CompilerLog, SyntaxErrorKind};

pub type LexerResult<T> = Result<T, LexerLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {
    EmptyCharLiteral { span: Span },
    ExpectedDecimalFloat { span: Span },
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
            LexerLog::ExpectedDecimalFloat { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedDecimalFloat, span),
            LexerLog::ExpectedTypeSuffix { span } => CompilerLog::syntax_err(SyntaxErrorKind::ExpectedTypeSuffix, span),
            LexerLog::LineBreakInCharLiteral { span } => CompilerLog::syntax_err(SyntaxErrorKind::LineBreakInCharLiteral, span),
            LexerLog::LineBreakInStrLiteral { span } => CompilerLog::syntax_err(SyntaxErrorKind::LineBreakInStrLiteral, span),
            LexerLog::TooLongCharLiteral { span } => CompilerLog::syntax_err(SyntaxErrorKind::TooLongCharLiteral, span),
            LexerLog::UnclosedCharLiteral { span } => CompilerLog::syntax_err(SyntaxErrorKind::UnclosedCharLiteral, span),
            LexerLog::UnclosedStrLiteral { span } => CompilerLog::syntax_err(SyntaxErrorKind::UnclosedStrLiteral, span),
            LexerLog::UnknownEscseq { span } => CompilerLog::syntax_err(SyntaxErrorKind::UnknownEscseq, span),
        }
    }
}
