use crate::lexer::token::Span;
use crate::log::{CompilerLog, TypeErrorKind};
use crate::hir::id::*;

pub type TypeResult<T> = Result<T, TypeLog>;

// todo: フィールドに span を追加
#[derive(Clone, Debug, PartialEq)]
pub enum TypeLog {
    FnCallWithInvalidArgLen { expected: usize, provided: usize },
    // todo: 引数を追加
    InconsistentConstraint,
    UndefinedType { type_id: TypeId },
    UnresolvedType { type_id: TypeId },
}

impl From<TypeLog> for CompilerLog {
    fn from(value: TypeLog) -> Self {
        match value {
            TypeLog::FnCallWithInvalidArgLen { expected, provided } => CompilerLog::type_err(
                TypeErrorKind::FnCallWithInvalidArgLen { expected, provided },
                Span::new(0, 0),
            ),
            TypeLog::InconsistentConstraint => CompilerLog::type_err(
                TypeErrorKind::InconsistentConstraint,
                Span::new(0, 0),
            ),
            TypeLog::UndefinedType { type_id } => CompilerLog::type_err(
                TypeErrorKind::UndefinedType { type_id },
                Span::new(0, 0),
            ),
            TypeLog::UnresolvedType { type_id } => CompilerLog::type_err(
                TypeErrorKind::UnresolvedType { type_id },
                Span::new(0, 0),
            ),
        }
    }
}
