use crate::parser::ast::Path;
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct InputTree {
    pub hakos: Vec<InputHako>,
    pub main_hako_name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputHako {
    pub id: HakoId,
    pub name: String,
    pub mods: Vec<InputMod>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputMod {
    pub id: ModId,
    pub path: Path,
    pub source: String,
    pub submods: Vec<InputMod>,
}
