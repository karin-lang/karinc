use crate::data::hir::item::*;
use self::entity::*;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum NameResolutionLog {
    UnknownGlobalSymbol(HirGlobalSymbol),
}

pub struct NameResolution {
    pub(crate) logs: Vec<NameResolutionLog>,
}

impl NameResolution {
    pub fn new() -> NameResolution {
        NameResolution {
            logs: Vec::new(),
        }
    }

    pub fn resolve(mut self, hir: &Hir) -> (HashMap<HirGlobalSymbol, HirSymbolAccessorMap>, Vec<NameResolutionLog>) {
        for (symbol, entity) in hir.global_entity_map.value() {
            match entity {
                HirGlobalEntity::ModuleDeclaration(_) => (),
                HirGlobalEntity::FunctionDeclaration(declaration) => self.resolve_function_declaration(&hir.global_entity_map, symbol, declaration),
            }
        }

        // for (each_module_symbol, each_module) in &hir.modules {
        //     let module_name_resolution = ModuleNameResolution::new();
        //     module_name_resolution.resolve_module(&hir.modules, each_module_symbol, each_module);
        // }

        (, self.logs)
    }

    pub fn resolve_function_declaration(&mut self, global_entity_map: &HirGlobalEntityMap, item_symbol: &HirDividedGlobalSymbol, declaration: &HirFunctionDeclaration) {
        let function_name_resolution = FunctionNameResolution::new();
        function_name_resolution.resolve_function_body(&declaration.exprs);
    }
}

pub struct ModuleNameResolution;

impl ModuleNameResolution {
    pub fn new() -> ModuleNameResolution {
        ModuleNameResolution
    }

    // pub fn resolve_module(mut self, global_symbol_map: &mut HirGlobalSymbolMap, hir_modules: &HashMap<HirDividedGlobalSymbol, HirModule>, module_symbol: &HirDividedGlobalSymbol, module: &HirModule) {
    //     self._resolve_module(global_symbol_map, hir_modules, module_symbol, module)
    // }

    // pub fn _resolve_module(&mut self, global_symbol_map: &mut HirGlobalSymbolMap, hir_modules: &HashMap<HirDividedGlobalSymbol, HirModule>, module_symbol: &HirDividedGlobalSymbol, module: &HirModule) {
    //     let entity = HirModuleEntity {
    //         items: module.items.keys().map(|v| v.clone().into()).collect(),
    //         submodules: module.submodules.iter().map(|v| v.clone().into()).collect(),
    //     };
    //     global_symbol_map.insert(module_symbol.clone().into(), HirGlobalEntity::Module(entity));

    //     for (each_item_symbol, each_item) in &module.items {
    //         self.resolve_item(global_symbol_map, each_item_symbol, each_item);
    //     }

    //     for submodule_symbol in &module.submodules {
    //         let submodule = match hir_modules.get(submodule_symbol) {
    //             Some(v) => v,
    //             None => panic!("hir module structure is broken: unknown submodule symbol"),
    //         };

    //         self._resolve_module(global_symbol_map, hir_modules, submodule_symbol, submodule);
    //     }
    // }

    // pub fn resolve_item(&mut self, global_symbol_map: &mut HirGlobalSymbolMap, item_symbol: &HirDividedGlobalSymbol, item: &HirItem) {
    //     match item {
    //         HirItem::FunctionDeclaration(declaration) => self.resolve_function_declaration(global_symbol_map, item_symbol, declaration),
    //     }
    // }

    // pub fn resolve_function_declaration(&mut self, global_symbol_map: &mut HirGlobalSymbolMap, item_symbol: &HirDividedGlobalSymbol, declaration: &HirFunctionDeclaration) {
    //     global_symbol_map.insert(item_symbol.clone().into(), HirGlobalEntity::Function);
    //     let function_name_resolution = FunctionNameResolution::new();
    //     function_name_resolution.resolve_function_body(&declaration.exprs);
    // }
}

// メモ：スコープとして記録するのは関数ボディ以下（大域シンボルは現在のパスを見て解決できる）
#[derive(Clone, Debug, PartialEq)]
pub struct NameResolutionScopeHierarchy {
    pub scopes: Vec<NameResolutionScope>,
}

impl NameResolutionScopeHierarchy {
    pub fn new() -> NameResolutionScopeHierarchy {
        NameResolutionScopeHierarchy {
            scopes: Vec::new(),
        }
    }

    pub fn declare_symbol(&mut self, symbol: HirLocalSymbol) {
        let current_scope = self.scopes.last_mut().expect("could not find any scope because scope layer is broken");
        current_scope.declare_symbol(symbol);
    }

    // メモ：メンバアクセスとパスは構文が違うので別管理
    // メモ：EntityをHIR生成時に登録するか、パスもメンバーアクセスも事後チェックにするか
    pub fn resolve_symbol(&mut self, accessor: &HirSymbolAccessor) -> HirSymbolCodeOrPath {
        match &accessor.kind {
            HirSymbolAccessorKind::SingleSegment(segment) => {
                match self.find_declared_id(segment) {
                    // note: 被修飾セグメントが局所識別子として宣言されていた場合はシンボルコードを返す
                    Some(v) => HirSymbolCodeOrPath::SymbolCode(v.code),
                    // note: そうでない場合はパスと自動判定して返す
                    None => HirSymbolCodeOrPath::Path(
                        HirPath { segments: vec![segment.clone()] },
                    ),
                }
            },
            HirSymbolAccessorKind::MultipleSegments(path, member_access_chain) => {
                match path.segments.get(0) {
                    Some(unqualified_id) => {
                        let unqualified_symbol = match self.find_declared_id(unqualified_id) {
                            Some(v) => v,
                            None => ,
                        }
                    },
                    // メモ：問題点：Entityが事前に生成されていないとメンバーアクセスが解決できない
                    None => ,
                }
            },
        }
    }

    pub(crate) fn resolve_path(&self, path: &HirPath) {}

    pub(crate) fn find_declared_id(&self, id: &str) -> Option<&HirLocalSymbol> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(v) = each_scope.declared_symbols.iter().rev().find(|v| v.id == *id) {
                return Some(v);
            }
        }

        None
    }

    fn _enter_scope(&mut self, self_symbol: Option<HirLocalSymbol>) {
        let child_scope = NameResolutionScope::new(self_symbol);
        self.scopes.push(child_scope);
    }

    pub fn enter_named_scope(&mut self, self_symbol: HirLocalSymbol) {
        self._enter_scope(Some(self_symbol));
    }

    pub fn enter_unnamed_scope(&mut self) {
        self._enter_scope(None);
    }

    pub fn exit_to_parent_scope(&mut self) {
        self.scopes.pop().expect("could not exit scope because scope layer is broken");
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NameResolutionScope {
    pub(crate) declared_symbols: Vec<HirLocalSymbol>,
}

impl NameResolutionScope {
    pub fn new(self_symbol: Option<HirLocalSymbol>) -> NameResolutionScope {
        NameResolutionScope {
            // note: 自身のスコープがシンボル化されている場合、その識別子をスコープ内部からの検索対象に含める (e.g. 関数定義, ブロックラベル)
            declared_symbols: match self_symbol {
                Some(v) => vec![v],
                None => Vec::new(),
            },
        }
    }

    pub fn declare_symbol(&mut self, symbol: HirLocalSymbol) {
        self.declared_symbols.push(symbol);
    }
}

pub struct FunctionNameResolution {
    pub scope_hierarchy: NameResolutionScopeHierarchy,
    pub symbol_accessor_map: HirSymbolAccessorMap,
}

impl FunctionNameResolution {
    pub fn new() -> FunctionNameResolution {
        FunctionNameResolution {
            scope_hierarchy: NameResolutionScopeHierarchy::new(),
            symbol_accessor_map: HirSymbolAccessorMap::new(),
        }
    }

    pub fn resolve_function_body(mut self, exprs: &Vec<HirExpression>) {
        for each_expr in exprs {
            self.resolve_expr(each_expr);
        }
    }

    pub fn resolve_expr(&mut self, expr: &HirExpression) {
        match expr {
            HirExpression::Number(_number_literal) => unimplemented!(),
            HirExpression::Symbol(symbol) => self.scope_hierarchy.resolve_symbol(symbol),
            HirExpression::VariableDeclaration(declaration) => self.resolve_variable_declaration(declaration),
            HirExpression::FunctionCall(call) => self.resolve_function_call(call),
        }
    }

    pub fn resolve_variable_declaration(&mut self, declaration: &HirVariableDeclaration) {
        let entity = HirVariableDeclaration {
            symbol: declaration.symbol.clone(),
            initial_expr: declaration.initial_expr.clone(),
        };
        self.local_symbol_map.insert(declaration.symbol.code, HirLocalEntity::Variable(entity));
        self.scope_hierarchy.declare_symbol(declaration.symbol.clone());
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
