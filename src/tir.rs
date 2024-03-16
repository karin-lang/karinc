pub mod lower;

use std::collections::HashMap;

use crate::parser::ast;

#[derive(Clone, Debug, PartialEq)]
pub struct Tir {
    pub bodies: HashMap<ast::GlobalSymbol, >,
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
