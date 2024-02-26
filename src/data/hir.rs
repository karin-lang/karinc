pub mod expr;
pub mod item;
pub mod symbol;

use std::collections::HashMap;

use self::{item::*, symbol::*};

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub modules: HashMap<HirGlobalSymbol, HirModule>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirModule {
    pub items: HashMap<HirGlobalSymbol, HirItem>,
    pub submodules: Vec<HirGlobalSymbol>,
}
