// pub mod check;
pub mod constraint;
// pub mod lower;
pub mod r#type;

use std::collections::HashMap;

use crate::parser::ast;
use crate::hir::id::*;
use crate::tir::r#type::Type;

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
    FnCall(FnCall),
    PathRef(Type),
    LocalDecl(LocalId),
    LocalRef(LocalId),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub r#fn: ast::Path,
    pub args: Vec<ActualArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ActualArg {
    pub expr: Expr,
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
    pub r#type: Type,
    pub init: Expr,
}
