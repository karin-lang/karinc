use std::collections::HashMap;

use crate::data::ast::*;
use crate::data::hir::{*, expr::*, item::*, path::*};

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

    pub fn lower_item(&mut self, node: &AstNode) -> Option<(HirDefId, HirItem)> {
        match node.name.as_ref() {
            "fn_dec" => self.lower_function_declaration(node).map(|(id, v)| (id, HirItem::FunctionDeclaration(v))),
            _ => {
                self.logs.push(HirLoweringLog::UnknownNodeId(node.name.clone()));
                None
            },
        }
    }

    pub fn lower_function_declaration(&mut self, node: &AstNode) -> Option<(HirDefId, HirFunctionDeclaration)> {
        let id_leaf = node.find("id").unwrap().expect_leaf();
        let id = HirDefId(id_leaf.value.kind.expect_id().clone());

        let expr_nodes = node.find("fn_exprs").unwrap().expect_node();
        let exprs = self.lower_function_expressions(expr_nodes);

        let declaration = HirFunctionDeclaration { exprs };
        Some((id, declaration))
    }

    pub fn lower_function_expressions(&mut self, node: &AstNode) -> Vec<HirExpression> {
        let mut exprs = Vec::new();

        for expr_child in &node.children {
            let new_expr = self.lower_expression(expr_child);

            if let Some(v) = new_expr {
                exprs.push(v);
            }
        }

        exprs
    }

    pub fn lower_expression(&mut self, child: &AstChild) -> Option<HirExpression> {
        match child.get_name() {
            "number" => self.lower_number_literal(child.expect_leaf()).map(|v| HirExpression::Number(v)),
            "fn_call" => unimplemented!(),
            _ => {
                self.logs.push(HirLoweringLog::UnknownNodeId(child.get_name().to_string()));
                None
            },
        }
    }

    pub fn lower_number_literal(&mut self, leaf: &AstLeaf) -> Option<HirNumberLiteral> {
        let value = leaf.value.kind.expect_number().0.clone();
        Some(HirNumberLiteral { value })
    }

    pub fn lower_function_call(&mut self, node: &AstNode) -> Option<HirFunctionCall> {
        let id_leaf = node.find("id").unwrap().expect_leaf();

        let id = HirRefPath {
            segments: vec![id_leaf.value.kind.expect_id().clone()],
            name_resolution_status: HirNameResolutionStatus::Unresolved,
        };

        let args = node
            .find("actual_fn_args")
            .map(|v| self.lower_actual_function_args(v.expect_node()))
            .unwrap_or(Vec::new());

        let function_call = HirFunctionCall { id, args };
        Some(function_call)
    }

    pub fn lower_actual_function_args(&mut self, node: &AstNode) -> Vec<HirActualFunctionArgument> {
        let mut args = Vec::new();

        for each_child in &node.children {
            let new_arg = self.lower_expression(each_child).map(|v| HirActualFunctionArgument::Expression(v));

            if let Some(v) = new_arg {
                args.push(v);
            }
        }

        args
    }
}
