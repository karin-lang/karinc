pub mod resolve;
pub mod table;
pub mod log;

use std::cell::{Ref, RefCell};
use std::rc::Rc;

use crate::parser::ast;
use crate::hir;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Ambiguous(AmbiguousType),
    Item(ast::Path),
    Prim(ast::PrimType),
    Fn(FnType),
    Unknown,
}

impl Type {
    // todo: テストを追加
    // 双方の型に一貫性があるか判断する; どちらか一方がもう一方の部分型である場合に一貫性があるとみなす
    //   ※同一の型は部分型としてみなされる
    pub fn is_consistent(&self, r#type: &Type) -> bool {
        self.is_subtype(r#type) || r#type.is_subtype(self)
    }

    // todo: テストを追加
    // type が self の部分型か判定する
    pub fn is_subtype(&self, r#type: &Type) -> bool {
        match self {
            Type::Ambiguous(ambiguous) => match r#type {
                Type::Prim(prim) => match ambiguous {
                    AmbiguousType::Num => match prim {
                        ast::PrimType::I8 | ast::PrimType::I16 | ast::PrimType::I32 | ast::PrimType::I64 |
                        ast::PrimType::U8 | ast::PrimType::U16 | ast::PrimType::U32 | ast::PrimType::U64 |
                        ast::PrimType::F32 | ast::PrimType::F64 => true,
                        _ => false,
                    },
                    AmbiguousType::Int => match prim {
                        ast::PrimType::I8 | ast::PrimType::I16 | ast::PrimType::I32 | ast::PrimType::I64 |
                        ast::PrimType::U8 | ast::PrimType::U16 | ast::PrimType::U32 | ast::PrimType::U64 => true,
                        _ => false,
                    },
                    AmbiguousType::Float => match prim {
                        ast::PrimType::F32 | ast::PrimType::F64 => true,
                        _ => false,
                    },
                },
                _ => false,
            },
            Type::Item(_) => self == r#type, // fix
            Type::Prim(_) => self == r#type, // check
            Type::Fn(_) => self == r#type, // check
            Type::Unknown => true,
        }
    }
}

impl From<&ast::Type> for Type {
    fn from(value: &ast::Type) -> Self {
        match &*value.kind {
            ast::TypeKind::Id(_) => unimplemented!(),
            ast::TypeKind::Prim(prim_type) => Type::Prim(prim_type.clone()),
        }
    }
}

impl From<&hir::Type> for Type {
    fn from(value: &hir::Type) -> Self {
        match &*value.kind {
            hir::TypeKind::Item(_) => unimplemented!(),
            hir::TypeKind::Prim(prim_type) => Type::Prim(prim_type.clone()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AmbiguousType {
    Num,
    Int,
    Float,
}

impl AmbiguousType {
    pub fn derive_default(&self) -> ast::PrimType {
        match self {
            AmbiguousType::Num | AmbiguousType::Int => ast::PrimType::I32,
            AmbiguousType::Float => ast::PrimType::F32,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnType {
    pub ret_type: Box<Type>,
    pub arg_types: Vec<Type>,
}

#[derive(PartialEq)]
pub struct TypePtr(Rc<RefCell<Type>>);

impl TypePtr {
    pub fn new(r#type: Type) -> TypePtr {
        TypePtr(Rc::new(RefCell::new(r#type)))
    }

    pub fn borrow(&self) -> Ref<Type> {
        (*self.0).borrow()
    }
}

impl Clone for TypePtr {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl std::fmt::Debug for TypePtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", *self.borrow())
    }
}
