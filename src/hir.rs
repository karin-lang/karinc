pub mod expr;
pub mod item;
pub mod symbol;

use std::collections::HashMap;

use crate::data::ast::*;
use crate::data::hir::{*, expr::*, symbol::*};

pub struct HirModuleContextLayer {
    pub symbol_index_generator: HirCounter<HirSymbolIndex>,
}

impl HirModuleContextLayer {
    pub fn new() -> HirModuleContextLayer {
        HirModuleContextLayer {
            symbol_index_generator: HirCounter::new(),
        }
    }
}

pub struct HirFunctionContextLayer {
    pub symbol_code_generator: HirCounter<HirSymbolCode>,
}

impl HirFunctionContextLayer {
    pub fn new() -> HirFunctionContextLayer {
        HirFunctionContextLayer {
            symbol_code_generator: HirCounter::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    UnknownNodeId(String),
}

pub struct HirLowering {
    pub(crate) module_context_hierarchy: Vec<HirModuleContextLayer>,
    pub(crate) function_context_hierarchy: Vec<HirFunctionContextLayer>,
    pub(crate) logs: Vec<HirLoweringLog>,
}

impl HirLowering {
    pub fn new() -> HirLowering {
        HirLowering {
            module_context_hierarchy: Vec::new(),
            function_context_hierarchy: Vec::new(),
            logs: Vec::new(),
        }
    }

    #[allow(unused)]
    pub(crate) fn add_module_context_layer(mut self) -> HirLowering {
        self.module_context_hierarchy.push(HirModuleContextLayer::new());
        self
    }

    #[allow(unused)]
    pub(crate) fn add_function_context_layer(mut self) -> HirLowering {
        self.function_context_hierarchy.push(HirFunctionContextLayer::new());
        self
    }

    pub fn lower(mut self, ast_container: &AstContainer) -> (Hir, Vec<HirLoweringLog>) {
        let mut modules = HashMap::new();

        for each_module in &ast_container.roots {
            let ((new_module_symbol, new_module), new_child_modules) = self.lower_module(each_module);
            modules.insert(new_module_symbol, new_module);

            for (each_new_module_symbol, each_new_module) in new_child_modules {
                modules.insert(each_new_module_symbol, each_new_module);
            }
        }

        let hir = Hir { global_entity_map };
        (hir, self.logs)
    }

    // note: モジュール木の走査結果を先行順に見せかけるため対象モジュールと子モジュールを分割して返す
    pub fn lower_module(&mut self, ast_module: &AstModule) -> ((HirDividedGlobalSymbol, HirModule), Vec<(HirDividedGlobalSymbol, HirModule)>) {
        self.enter_module_context();

        let mut submodules = Vec::new();
        let mut submodule_symbols = Vec::new();
        let mut items = HashMap::new();
        let module_path = HirPath { segments: ast_module.path.clone() };

        for each_child in &ast_module.ast.root.children {
            if let Some((new_item_symbol, new_item)) = self.lower_item(module_path.clone(), each_child.expect_node()) {
                items.insert(new_item_symbol, new_item);
            }
        }

        for each_submodule in &ast_module.submodules {
            let new_submodule_symbol = HirDividedGlobalSymbol::from_module_path(each_submodule.path.clone());
            submodule_symbols.push(new_submodule_symbol);

            let (new_child_module, new_grandchild_modules) = self.lower_module(each_submodule);
            submodules.push(new_child_module);

            for each_new_grandchild_module in new_grandchild_modules {
                submodules.push(each_new_grandchild_module);
            }
        }

        let target_module_symbol = HirDividedGlobalSymbol::from_module_path(ast_module.path.clone());
        let target_module = HirModule { items, submodules: submodule_symbols };

        self.exit_module_context();

        ((target_module_symbol, target_module), submodules)
    }

    fn enter_module_context(&mut self) {
        self.module_context_hierarchy.push(HirModuleContextLayer::new());
    }

    fn exit_module_context(&mut self) {
        self.module_context_hierarchy.pop().expect("could not exit module context because the hierarchy is broken");
    }

    fn generate_symbol_index(&mut self) -> HirSymbolIndex {
        let layer = self.module_context_hierarchy.last_mut().expect("could not find symbol index generator because the hierarchy is broken");
        layer.symbol_index_generator.generate()
    }

    fn enter_function_context(&mut self) {
        self.function_context_hierarchy.push(HirFunctionContextLayer::new());
    }

    fn exit_function_context(&mut self) {
        self.function_context_hierarchy.pop().expect("could not exit function context because the hierarchy is broken");
    }

    fn generate_symbol_code(&mut self) -> HirSymbolCode {
        let layer = self.function_context_hierarchy.last_mut().expect("could not find symbol index generator because the hierarchy is broken");
        layer.symbol_code_generator.generate()
    }
}
