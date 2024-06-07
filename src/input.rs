use crate::parser::ast::Path;
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct InputTree {
    pub hakos: Vec<HakoInput>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HakoInput {
    pub id: HakoId,
    pub mods: Vec<ModInput>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModInput {
    pub id: ModId,
    pub path: Path,
    pub source: String,
}
