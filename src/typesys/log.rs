use crate::lexer::token::Span;
use crate::log::{CompilerLog, TypeErrorKind};
use crate::hir::id::*;

pub type TypeResult<T> = Result<T, TypeLog>;

// todo: フィールドに span を追加
#[derive(Clone, Debug, PartialEq)]
pub enum TypeLog {
    ExpectedMainFnArgsToBeZeroLen { span: Span },
    ExpectedMainFnRetTypeToBeNone { span: Span },
    FnCallWithInvalidArgLen { expected: usize, provided: usize },
    // todo: 引数を追加
    InconsistentConstraint,
    MainFnIsNotFound,
    UnknownType { type_id: TypeId },
}

impl From<TypeLog> for CompilerLog {
    fn from(value: TypeLog) -> Self {
        match value {
            TypeLog::ExpectedMainFnArgsToBeZeroLen { span } => CompilerLog::type_err(
                TypeErrorKind::ExpectedMainFnArgsToBeZeroLen,
                span,
            ),
            TypeLog::ExpectedMainFnRetTypeToBeNone { span } => CompilerLog::type_err(
                TypeErrorKind::ExpectedMainFnRetTypeToBeNone,
                span,
            ),
            TypeLog::FnCallWithInvalidArgLen { expected, provided } => CompilerLog::type_err(
                TypeErrorKind::FnCallWithInvalidArgLen { expected, provided },
                Span::new(0, 0), /* fix span */
            ),
            TypeLog::InconsistentConstraint => CompilerLog::type_err(
                TypeErrorKind::InconsistentConstraint,
                Span::new(0, 0),
            ),
            TypeLog::MainFnIsNotFound => CompilerLog::type_err(
                TypeErrorKind::MainFnIsNotFound,
                Span::new(0, 0),
            ),
            TypeLog::UnknownType { type_id } => CompilerLog::type_err(
                TypeErrorKind::UnknownType { type_id },
                Span::new(0, 0),
            ),
        }
    }
}
