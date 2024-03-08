pub mod lowering;

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub global_entities: HashMap<GlobalEntityId, GlobalEntity>,
}

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct GlobalEntityId {
    pub segments: Vec<String>,
}

impl GlobalEntityId {
    pub fn add_segment(mut self, segment: String) -> GlobalEntityId {
        self.segments.push(segment);
        self
    }
}

impl From<Vec<&str>> for GlobalEntityId {
    fn from(value: Vec<&str>) -> Self {
        Self { segments: value.iter().map(|v| v.to_string()).collect() }
    }
}

impl From<Vec<String>> for GlobalEntityId {
    fn from(value: Vec<String>) -> Self {
        Self { segments: value }
    }
}

impl From<Vec<&String>> for GlobalEntityId {
    fn from(value: Vec<&String>) -> Self {
        Self { segments: value.iter().map(|v| (*v).clone()).collect() }
    }
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
    LocalEntity(LocalEntityId),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl;

#[derive(Clone, Debug, PartialEq)]
pub struct VarInit;
