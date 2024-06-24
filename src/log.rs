use crate::lexer::token::Span;
use crate::parser::ast;
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct CompilerLog {
    pub kind: CompilerLogKind,
    pub span: Span,
}

impl CompilerLog {
    #[inline(always)]
    pub fn warn(warn: CompilerWarn, span: Span) -> CompilerLog {
        CompilerLog { kind: CompilerLogKind::Warn(warn), span }
    }

    #[inline(always)]
    pub fn err(err: CompilerErr, span: Span) -> CompilerLog {
        CompilerLog { kind: CompilerLogKind::Err(err), span }
    }

    #[inline(always)]
    pub fn syntax_err(kind: SyntaxErrorKind, span: Span) -> CompilerLog {
        CompilerLog {
            kind: CompilerLogKind::Err(
                CompilerErr::SyntaxError { kind },
            ),
            span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerLogKind {
    Err(CompilerErr),
    Warn(CompilerWarn),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerErr {
    GlobalIdIsNotFound { global_id: GlobalId },
    PathIsNotFoundInScope { path: ast::Path },
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
