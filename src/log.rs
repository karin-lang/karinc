use crate::lexer::token::Span;
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct CompilerLog {
    pub kind: CompilerLogKind,
    pub span: Span,
}

impl CompilerLog {
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
