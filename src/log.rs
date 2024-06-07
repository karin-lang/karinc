use crate::lexer::{token::Span, tokenize::LexerLog};
use crate::parser::ParserLog;
use crate::hir::{id::*, lower::HirLoweringLog};

#[derive(Clone, Debug, PartialEq)]
pub struct CompilerLog {
    pub kind: CompilerLogKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerLogKind {
    Err(CompilerErr),
    Warn(CompilerWarn),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerErr {
    SyntaxError { kind: SyntaxErrorKind },
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerWarn {
}

#[derive(Clone, Debug, PartialEq)]
pub enum SyntaxErrorKind {
    EmptyCharLiteral,
    ExpectedExpr,
    ExpectedExprButFoundHako { hako_id: HakoId },
    ExpectedExprButFoundMod { mod_id: ModId },
}

impl From<LexerLog> for CompilerLog {
    fn from(value: LexerLog) -> Self {
        let (kind, span) = match value {
            LexerLog::EmptyCharLiteral { span } => (
                CompilerLogKind::Err(
                    CompilerErr::SyntaxError {
                        kind: SyntaxErrorKind::EmptyCharLiteral,
                    },
                ),
                span,
            ),
            _ => unimplemented!(),
        };
        Self { kind, span }
    }
}

impl From<ParserLog> for CompilerLog {
    fn from(value: ParserLog) -> Self {
        let (kind, span) = match value {
            ParserLog::ExpectedExpr { span } => (
                CompilerLogKind::Err(
                    CompilerErr::SyntaxError {
                        kind: SyntaxErrorKind::ExpectedExpr,
                    },
                ),
                span,
            ),
            _ => unimplemented!(),
        };
        Self { kind, span }
    }
}

impl From<HirLoweringLog> for CompilerLog {
    fn from(value: HirLoweringLog) -> Self {
        let (kind, span) = match value {
            HirLoweringLog::ExpectedExprButFoundHako { hako_id, span } => (
                CompilerLogKind::Err(
                    CompilerErr::SyntaxError {
                        kind: SyntaxErrorKind::ExpectedExprButFoundHako { hako_id },
                    },
                ),
                span,
            ),
            HirLoweringLog::ExpectedExprButFoundMod { mod_id, span } => (
                CompilerLogKind::Err(
                    CompilerErr::SyntaxError {
                        kind: SyntaxErrorKind::ExpectedExprButFoundMod { mod_id },
                    },
                ),
                span,
            ),
        };
        Self { kind, span }
    }
}
