use std::collections::HashMap;

use crate::data::hir::{*, expr::*};

use self::item::HirFunctionDeclaration;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct HirGlobalEntityMap(HashMap<HirGlobalSymbol, HirGlobalEntity>);

impl HirGlobalEntityMap {
    pub fn new() -> HirGlobalEntityMap {
        HirGlobalEntityMap(HashMap::new())
    }

    pub fn value(&self) -> &HashMap<HirGlobalSymbol, HirGlobalEntity> {
        &self.0
    }

    pub fn insert(&mut self, symbol: HirGlobalSymbol, entity: HirGlobalEntity) {
        self.0.insert(symbol, entity);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirGlobalEntity {
    ModuleDeclaration(HirModule),
    FunctionDeclaration(HirFunctionDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirLocalEntityMap(HashMap<HirSymbolCode, HirLocalEntity>);

impl HirLocalEntityMap {
    pub fn new() -> HirLocalEntityMap {
        HirLocalEntityMap(HashMap::new())
    }

    pub fn insert(&mut self, symbol_code: HirSymbolCode, entity: HirLocalEntity) {
        self.0.insert(symbol_code, entity);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirLocalEntity {
    VariableDeclaration(HirVariableDeclaration),
}
