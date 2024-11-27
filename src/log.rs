use crate::lexer::token::{self, Span};
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

    #[inline(always)]
    pub fn type_err(kind: TypeErrorKind, span: Span) -> CompilerLog {
        CompilerLog {
            kind: CompilerLogKind::Err(
                CompilerErr::TypeError { kind },
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
    DuplicateItemName { id: ast::Id },
    DuplicateMarker { name: String },
    GlobalIdIsNotFound { global_id: GlobalId },
    PathIsNotFoundInScope { path: ast::Path },
    IdIsNotFoundInScope { id: ast::Id },
    SyntaxError { kind: SyntaxErrorKind },
    TypeError { kind: TypeErrorKind },
    UnknownSysEmbedName { name: String },
    UnnecessaryPath { path: ast::Path },
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompilerWarn {
}

#[derive(Clone, Debug, PartialEq)]
pub enum SyntaxErrorKind {
    /* lexer */
    EmptyCharLiteral,
    ExpectedDecimalFloat,
    ExpectedTypeSuffix,
    LineBreakInCharLiteral,
    LineBreakInStrLiteral,
    TooLongCharLiteral,
    UnclosedCharLiteral,
    UnclosedStrLiteral,
    UnknownEscseq,

    /* parser */
    ExpectedActualArg,
    ExpectedExpr,
    ExpectedFormalArg,
    ExpectedId,
    ExpectedItem,
    ExpectedKeyword { keyword: token::Keyword },
    ExpectedStrLiteral,
    ExpectedToken { kind: token::TokenKind },
    ExpectedType,
    MarkerCannotTreatAsExpr { name: String },
    MarkerCannotTreatAsItemDescriptor { name: String },
    UnexpectedEof,
    UnknownMarkerName,

    /* HIR lowering */
    ExpectedExprButFoundHako { hako_id: HakoId },
    ExpectedExprButFoundMod { mod_id: ModId },
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeErrorKind {
    ExpectedMainFnArgsToBeZeroLen,
    ExpectedMainFnRetTypeToBeNone,
    FnCallWithInvalidArgLen { expected: usize, provided: usize },
    InconsistentConstraint,
    MainFnIsNotFound,
    UnknownType { type_id: TypeId },
}
