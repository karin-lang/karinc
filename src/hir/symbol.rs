use std::rc::Rc;

use crate::data::hir::item::*;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct NameResolutionScopeHierarchy {
    pub(crate) scopes: Vec<Rc<NameResolutionScope>>,
}

impl NameResolutionScopeHierarchy {
    pub fn new() -> NameResolutionScopeHierarchy {
        NameResolutionScopeHierarchy {
            scopes: Vec::new(),
        }
    }

    pub fn resolve_symbol(&mut self, accessor: &HirSymbolAccessor) {
        // let unqualified = accessor.segments.;

        // for each_symbol in self.
    }

    fn _enter_scope(&mut self, self_symbol: Option<(String, HirSymbolCode)>) {
        let parent = self.scopes.last().map(|v| Rc::clone(v));
        let child_scope = NameResolutionScope::new(parent, self_symbol);
        self.scopes.push(Rc::new(child_scope));
    }

    pub fn enter_named_scope(&mut self, self_id: String, self_symbol_code: HirSymbolCode) {
        self._enter_scope(Some((self_id, self_symbol_code)));
    }

    pub fn enter_unnamed_scope(&mut self) {
        self._enter_scope(None);
    }

    pub fn exit_to_parent_scope(&mut self) {
        self.scopes.pop().expect("could not exit scope because scope layer is broken");
    }
}

// メモ：スコープとして記録するのは関数ボディ以下（大域シンボルは現在のパスを見て解決できる）
#[derive(Clone, Debug, PartialEq)]
pub struct NameResolutionScope {
    pub parent: Option<Rc<NameResolutionScope>>,
    pub defined_symbols: Vec<(String, HirSymbolCode)>,
}

impl NameResolutionScope {
    pub fn new(parent: Option<Rc<NameResolutionScope>>, self_symbol: Option<(String, HirSymbolCode)>) -> NameResolutionScope {
        NameResolutionScope {
            parent,
            // note: 自身のスコープがシンボル化されている場合、その識別子をスコープ内部からの検索対象に含める (e.g. 関数定義, ブロックラベル)
            defined_symbols: match self_symbol {
                Some(v) => vec![v],
                None => Vec::new(),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NameResolutionMap {
    pub global_symbol_map: GlobalSymbolMap,
    // note: 関数定義シンボルと局所シンボルマップの組み合わせ
    pub local_symbol_maps: HashMap<HirGlobalSymbol, LocalSymbolMap>,
    // note: モジュールシンボルとシンボルアクセサマップの組み合わせ
    pub symbol_accessor_maps: HashMap<HirGlobalSymbol, SymbolAccessorMap>,
}

impl NameResolutionMap {
    pub fn new() -> NameResolutionMap {
        NameResolutionMap {
            global_symbol_map: GlobalSymbolMap::new(),
            local_symbol_maps: HashMap::new(),
            symbol_accessor_maps: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GlobalSymbolMap(HashMap<Vec<String>, HirGlobalEntity>);

impl GlobalSymbolMap {
    pub fn new() -> GlobalSymbolMap {
        GlobalSymbolMap(HashMap::new())
    }
}

// 場所ずらす？
#[derive(Clone, Debug, PartialEq)]
pub enum HirGlobalEntity {
    Variable(HirVariableDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalSymbolMap(HashMap<HirSymbolCode, HirLocalEntity>);

// 場所ずらす？
#[derive(Clone, Debug, PartialEq)]
pub enum HirLocalEntity {
    Variable(HirVariableDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolAccessorMap(HashMap<HirSymbolIndex, HirSymbolCodeOrPath>);

#[derive(Clone, Debug, PartialEq)]
pub enum NameResolutionLog {
    UnknownGlobalSymbol(HirGlobalSymbol),
}

pub struct NameResolution {
    pub(crate) scope_hierarchy: NameResolutionScopeHierarchy,
    pub(crate) map: NameResolutionMap,
    pub(crate) logs: Vec<NameResolutionLog>,
}

impl NameResolution {
    pub fn new() -> NameResolution {
        NameResolution {
            scope_hierarchy: NameResolutionScopeHierarchy::new(),
            map: NameResolutionMap::new(),
            logs: Vec::new(),
        }
    }

    pub fn resolve(mut self, hir: &Hir) -> (NameResolutionMap, Vec<NameResolutionLog>) {
        for (each_module_symbol, each_module) in &hir.modules {
            self.resolve_module(&hir.modules, each_module_symbol, each_module);
        }

        (self.map, self.logs)
    }

    pub fn resolve_module(&mut self, hir_modules: &HashMap<HirDividedGlobalSymbol, HirModule>, module_symbol: &HirDividedGlobalSymbol, module: &HirModule) {
        for (_, each_item) in &module.items {
            self.resolve_item(each_item);
        }

        for submodule_symbol in &module.submodules {
            let submodule = match hir_modules.get(submodule_symbol) {
                Some(v) => v,
                None => panic!("hir module structure is broken: unknown submodule symbol"),
            };

            self.resolve_module(hir_modules, submodule_symbol, submodule);
        }
    }

    pub fn resolve_item(&mut self, item: &HirItem) {
        match item {
            HirItem::FunctionDeclaration(declaration) => self.resolve_function_declaration(declaration),
        }
    }

    pub fn resolve_function_declaration(&mut self, declaration: &HirFunctionDeclaration) {
        for each_expr in &declaration.exprs {
            self.resolve_expr(each_expr);
        }
    }

    pub fn resolve_expr(&mut self, expr: &HirExpression) {
        match expr {
            HirExpression::Number(_number_literal) => unimplemented!(),
            HirExpression::Symbol(_symbol) => unimplemented!(),
            HirExpression::VariableDeclaration(_declaration) => unimplemented!(),
            HirExpression::FunctionCall(call) => self.resolve_function_call(call),
        }
    }

    pub fn resolve_function_call(&mut self, call: &HirFunctionCall) {
        self.scope_hierarchy.resolve_symbol(&call.symbol);

        for each_arg in &call.args {
            match each_arg {
                HirActualFunctionArgument::Expression(expr) => self.resolve_expr(expr),
            }
        }
    }
}
