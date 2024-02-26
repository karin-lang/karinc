use std::rc::Rc;

use crate::data::hir::item::{HirFunctionDeclaration, HirItem};
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct NameResolutionScope {
    pub parent: Option<Rc<NameResolutionScope>>,
    pub is_module: bool,
    pub id: Option<String>,
    pub code: HirSymbolCode,
    pub defined_ids: Vec<String>,
}

impl NameResolutionScope {
    pub fn new(parent: Option<Rc<NameResolutionScope>>, is_module: bool, id: Option<String>, code: HirSymbolCode) -> NameResolutionScope {
        NameResolutionScope {
            parent,
            is_module,
            // id,
            // code,
            defined_ids: Vec::new(),
        }
    }
}

// 出力：参照シンボルコード ＋ 定義シンボル（シンボルコードもしくは絶対パス） のマップ ... モジュールごと

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolRelationMap(pub HashMap<HirSymbolCode, HirSymbol>);

// #[derive(Clone, Debug, PartialEq)]
// pub struct NameResolutionMap {
//     pub modules: HashMap<HirDefPath, >,
// }

#[derive(Clone, Debug, PartialEq)]
pub enum NameResolutionLog {
    UnknownPath(HirDefPath),
}

pub struct NameResolution {
    // pub(crate) map: NameResolutionMap,
    pub(crate) scopes: Vec<Rc<NameResolutionScope>>,
    pub(crate) logs: Vec<NameResolutionLog>,
}

impl NameResolution {
    pub fn new() -> NameResolution {
        NameResolution {
            // map: NameResolutionMap::new(),
            scopes: Vec::new(),
            logs: Vec::new(),
        }
    }

    pub fn resolve(mut self, hir: &Hir) -> Vec<NameResolutionLog> {
        for (each_module_path, each_module) in &hir.modules {
            self.resolve_module(&hir.modules, each_module_path, each_module);
        }

        self.logs
    }

    pub fn resolve_module(&mut self, module_map: &HashMap<HirDefPath, HirModule>, module_path: &HirDefPath, module: &HirModule) {
        let module_id = module_path.0.last().expect("module path of zero length segments is not allowed");
        self.enter_child_scope(true, Some(module_id.clone()));

        for (each_item_id, each_item) in &module.items {
            self.resolve_item(each_item_id, each_item);
        }

        for submodule_path in &module.submodules {
            let submodule = match module_map.get(submodule_path) {
                Some(v) => v,
                None => {
                    self.logs.push(NameResolutionLog::UnknownPath(submodule_path.clone()));
                    return;
                },
            };

            self.resolve_module(module_map, submodule_path, submodule);
        }

        self.exit_to_parent_scope();
    }

    pub fn resolve_item(&mut self, item_id: &HirDefId, item: &HirItem) {
        match item {
            HirItem::FunctionDeclaration(declaration) => self.resolve_function_declaration(item_id, declaration),
        }
    }

    pub fn resolve_function_declaration(&mut self, item_id: &HirDefId, declaration: &HirFunctionDeclaration) {
        self.enter_child_scope(false, Some(item_id.0.clone()));

        for each_expr in &declaration.exprs {
            self.resolve_expr(each_expr);
        }

        self.exit_to_parent_scope();
    }

    pub fn resolve_expr(&mut self, expr: &HirExpression) {
        match expr {
            HirExpression::Number(number_literal) => unimplemented!(),
            HirExpression::Symbol(symbol) => unimplemented!(),
            HirExpression::VariableDeclaration(declaration) => unimplemented!(),
            HirExpression::FunctionCall(call) => self.resolve_function_call(),
        }
    }

    pub fn resolve_function_call(&mut self, call: &HirFunctionCall) {
        
    }

    pub fn enter_child_scope(&mut self, is_module: bool, id: Option<String>) {
        let parent = self.scopes.last().map(|v| Rc::clone(v));
        let child_scope = NameResolutionScope::new(parent, is_module, id);
        self.scopes.push(Rc::new(child_scope));
    }

    pub fn exit_to_parent_scope(&mut self) {
        self.scopes.pop().expect("could not exit scope because scope layer is broken");
    }
}
