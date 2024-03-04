pub mod expr;
pub mod item;
pub mod symbol;

use self::{entity::*, symbol::*};

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub global_entity_map: HirGlobalEntityMap,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirModule {
    pub items: Vec<HirDividedGlobalSymbol>,
    pub submodules: Vec<HirDividedGlobalSymbol>,
}
