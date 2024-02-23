pub mod expr;
pub mod item;
pub mod path;

use std::collections::HashMap;

use self::{item::*, path::*};

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub modules: HashMap<HirDefPath, HirModule>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirModule {
    pub items: HashMap<HirDefId, HirItem>,
    pub submodules: Vec<HirDefPath>,
}
