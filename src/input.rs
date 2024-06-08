use crate::parser::ast::Path;
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct InputTree {
    pub hakos: Vec<InputHako>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputHako {
    pub id: HakoId,
    pub mods: Vec<InputMod>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputMod {
    pub id: ModId,
    pub path: Path,
    pub source: String,
    pub submods: Vec<InputMod>,
}
