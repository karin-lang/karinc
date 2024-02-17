use crate::data::{ast::*, hir::{expr::*, item::*, Hir}};

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

    pub fn lower(mut self, ast: &Ast) -> (Hir, Vec<HirLoweringLog>) {
        let mut items = Vec::new();

        for each_child in &ast.root.children {
            let new_item = self.lower_item(each_child.expect_node());

            if let Some(v) = new_item {
                items.push(v);
            }
        }

        let hir = Hir { items };
        (hir, self.logs)
    }

    pub fn lower_item(&mut self, node: &AstNode) -> Option<HirItem> {
        match node.name.as_ref() {
            "fn_dec" => self.lower_function_declaration(node).map(|v| HirItem::FunctionDeclaration(v)),
            _ => {
                self.logs.push(HirLoweringLog::UnknownNodeId(node.name.clone()));
                None
            },
        }
    }

    pub fn lower_function_declaration(&mut self, node: &AstNode) -> Option<HirFunctionDeclaration> {
        let id_leaf = node.find("id").unwrap().expect_leaf();
        let id = HirId(id_leaf.value.kind.expect_id().clone());

        let expr_nodes = node.find("fn_exprs").unwrap().expect_node();
        let exprs = self.lower_function_expressions(expr_nodes);

        let declaration = HirFunctionDeclaration { id, exprs };
        Some(declaration)
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
                return None;
            },
        }
    }

    pub fn lower_number_literal(&mut self, leaf: &AstLeaf) -> Option<HirNumberLiteral> {
        let value = leaf.value.kind.expect_number().0.clone();
        Some(HirNumberLiteral { value })
    }

    pub fn lower_function_call(&mut self, node: &AstNode) -> Option<HirFunctionCall> {
        let id_leaf = node.find("id").unwrap().expect_leaf();
        let id = HirId(id_leaf.value.kind.expect_id().clone());

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
