pub mod lower;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{hir, parser::ast};

#[derive(Clone, Debug, PartialEq)]
pub struct Tir {
    pub bodies: HashMap<ast::Path, Body>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub locals: Vec<Local>,
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    // todo: フィールドの情報が必要か判断
    PathRef(Type),
    LocalDecl(hir::LocalId),
    LocalRef(hir::LocalId),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Local {
    FormalArg(FormalArg),
    VarDecl(VarDecl),
    VarInit(VarInit),
    Undefined,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub r#type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub r#type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarInit {
    pub init: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub kind: Rc<RefCell<TypeKind>>,
}

impl Type {
    pub fn new(kind: TypeKind) -> Type {
        Type { kind: Rc::new(RefCell::new(kind)) }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Item(ast::Path),
    PrimType(ast::PrimType),
    Infer,
    Undefined,
}
