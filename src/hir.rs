use std::collections::HashMap;

use crate::parser::ast;

pub mod lower;
pub mod resolve;

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: HashMap<ast::Path, Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub args: Vec<LocalId>,
    pub body: Body,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub locals: Vec<Local>,
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct DivPath {
    pub item_path: ast::Path,
    pub following_path: ast::Path,
}

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct LocalId {
    id: usize,
}

impl From<usize> for LocalId {
    fn from(value: usize) -> Self {
        Self { id: value }
    }
}

impl From<&usize> for LocalId {
    fn from(value: &usize) -> Self {
        Self { id: *value }
    }
}

impl From<LocalId> for usize {
    fn from(value: LocalId) -> Self {
        value.id
    }
}

impl From<&LocalId> for usize {
    fn from(value: &LocalId) -> Self {
        value.id
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Local {
    FormalArg(FormalArg),
    VarDecl(VarDecl),
    VarInit(VarInit),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    FnCall(FnCall),
    PathRef(DivPath),
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
pub struct FormalArg;

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarInit {
    pub mutable: bool,
    pub init: Expr,
}
