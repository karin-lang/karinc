use super::*;
use crate::data::ast::*;
use crate::data::hir::{item::*, symbol::*};

impl HirLowering {
    pub fn lower_item(&mut self, parent_module_path: Vec<String>, node: &AstNode) -> Option<(HirGlobalSymbol, HirItem)> {
        match node.name.as_ref() {
            "fn_dec" => self.lower_function_declaration(parent_module_path, node).map(|(id, v)| (id, HirItem::FunctionDeclaration(v))),
            _ => {
                self.logs.push(HirLoweringLog::UnknownNodeId(node.name.clone()));
                None
            },
        }
    }

    pub fn lower_function_declaration(&mut self, parent_module_path: Vec<String>, node: &AstNode) -> Option<(HirGlobalSymbol, HirFunctionDeclaration)> {
        let id_leaf = node.find("id").unwrap().expect_leaf();
        let id = id_leaf.value.kind.expect_id().clone();
        let symbol = HirGlobalSymbol::new_with_parent(parent_module_path, id);

        let expr_nodes = node.find("fn_exprs").unwrap().expect_node();
        let exprs = self.lower_function_body(expr_nodes);

        let declaration = HirFunctionDeclaration { exprs };
        Some((symbol, declaration))
    }
}
