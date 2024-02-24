pub mod expr;
pub mod item;

use std::collections::HashMap;

use crate::data::ast::*;
use crate::data::hir::{*, expr::*, path::*};

pub struct HirModuleContextLayer {
    pub symbol_code_generator: HirSymbolCodeGenerator,
}

impl HirModuleContextLayer {
    pub fn new() -> HirModuleContextLayer {
        HirModuleContextLayer {
            symbol_code_generator: HirSymbolCodeGenerator::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    UnknownNodeId(String),
}

pub struct HirLowering {
    pub(crate) logs: Vec<HirLoweringLog>,
    pub(crate) module_context_layers: Vec<HirModuleContextLayer>,
}

impl HirLowering {
    pub fn new() -> HirLowering {
        HirLowering {
            logs: Vec::new(),
            module_context_layers: Vec::new(),
        }
    }

    #[allow(unused)]
    pub(crate) fn new_l1_context() -> HirLowering {
        HirLowering {
            logs: Vec::new(),
            module_context_layers: vec![HirModuleContextLayer::new()],
        }
    }

    pub fn lower(mut self, ast_container: &AstContainer) -> (Hir, Vec<HirLoweringLog>) {
        let mut modules = HashMap::new();

        for each_module in &ast_container.roots {
            let ((new_module_path, new_module), new_child_modules) = self.lower_module(each_module);
            modules.insert(new_module_path, new_module);

            for (each_new_module_path, each_new_module) in new_child_modules {
                modules.insert(each_new_module_path, each_new_module);
            }
        }

        let hir = Hir { modules };
        (hir, self.logs)
    }

    // note: モジュール木の走査結果を先行順に見せかけるため対象モジュールと子モジュールを分割して返す
    pub fn lower_module(&mut self, ast_module: &AstModule) -> ((HirDefPath, HirModule), Vec<(HirDefPath, HirModule)>) {
        self.enter_module_context();

        let mut submodules = Vec::new();
        let mut submodule_paths = Vec::new();
        let mut items = HashMap::new();

        for each_child in &ast_module.ast.root.children {
            if let Some((new_item_path, new_item)) = self.lower_item(each_child.expect_node()) {
                items.insert(new_item_path, new_item);
            }
        }

        for each_submodule in &ast_module.submodules {
            submodule_paths.push(HirDefPath(each_submodule.path.clone()));

            let (new_child_module, new_grandchild_modules) = self.lower_module(each_submodule);
            submodules.push(new_child_module);

            for each_new_grandchild_module in new_grandchild_modules {
                submodules.push(each_new_grandchild_module);
            }
        }

        let target_module_path = HirDefPath(ast_module.path.clone());
        let target_module = HirModule { items, submodules: submodule_paths };

        self.exit_module_context();

        ((target_module_path, target_module), submodules)
    }

    fn enter_module_context(&mut self) {
        self.module_context_layers.push(HirModuleContextLayer::new());
    }

    fn exit_module_context(&mut self) {
        self.module_context_layers.pop().expect("could not exit module context because it is broken");
    }

    fn generate_symbol_code(&mut self) -> HirSymbolCode {
        let layer = self.module_context_layers.last_mut().expect("could not get symbol code generator because module context is broken");
        layer.symbol_code_generator.generate()
    }
}
