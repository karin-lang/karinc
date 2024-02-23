pub mod expr;
pub mod item;

use std::collections::HashMap;

use crate::data::ast::*;
use crate::data::hir::{*, expr::*, path::*};

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    UnknownNodeId(String),
}

pub struct HirLowering {
    pub(crate) logs: Vec<HirLoweringLog>,
}

impl HirLowering {
    pub fn new() -> HirLowering {
        HirLowering {
            logs: Vec::new(),
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

        ((target_module_path, target_module), submodules)
    }
}
