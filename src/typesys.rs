pub mod constraint;
pub mod log;

use std::cell::{Ref, RefCell};
use std::rc::Rc;

use crate::parser::ast;
use crate::hir;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Infer(InferType),
    Item(ast::Path),
    Prim(ast::PrimType),
    Fn(FnType),
    Undefined,
    Unresolved,
}

impl Type {
    pub fn is_resolved(&self) -> bool {
        *self != Type::Unresolved
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
pub enum InferType {
    Num,
    Int,
    Float,
}

impl InferType {
    pub fn derive_default(&self) -> ast::PrimType {
        match self {
            InferType::Num | InferType::Int => ast::PrimType::I32,
            InferType::Float => ast::PrimType::F32,
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
