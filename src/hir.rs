pub mod lowering;

use std::collections::HashMap;

use crate::parser::ast;

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub global_entities: HashMap<ast::GlobalSymbol, GlobalEntity>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum GlobalEntity {
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub entities: HashMap<LocalEntityId, LocalEntity>,
    pub args: Vec<LocalEntityId>,
    pub body: Vec<Expr>,
}

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct LocalEntityId {
    id: usize,
}

impl From<usize> for LocalEntityId {
    fn from(value: usize) -> Self {
        Self { id: value }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LocalEntity {
    FormalArg(FormalArg),
    VarDecl(VarDecl),
    VarInit(VarInit),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    LocalEntity(),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl;

#[derive(Clone, Debug, PartialEq)]
pub struct VarInit;
