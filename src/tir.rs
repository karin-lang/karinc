pub mod lower;

use std::collections::HashMap;

use crate::parser::ast;

#[derive(Clone, Debug, PartialEq)]
pub struct Tir {
    pub hako: HashMap<ast::GlobalSymbol, Hako>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Hako {
    pub entities: HashMap<ast::GlobalSymbol, Entity>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Entity {
    Module(Module),
    Fn(Fn),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub children: Vec<ast::Id>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fn {
    pub args: Vec<FormalArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub r#type: Type,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Id(ast::Id),
    Prim(ast::PrimType),
}
