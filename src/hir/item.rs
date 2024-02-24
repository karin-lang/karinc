use super::*;
use crate::data::ast::*;
use crate::data::hir::{item::*, path::*};

impl HirLowering {
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
}
