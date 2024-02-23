use super::*;
use crate::data::ast::*;

impl HirLowering {
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
            "var_dec" => unimplemented!(),
            "fn_call" => unimplemented!(),
            "id_or_path" => unimplemented!(),
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

        let id = HirNameResolutionTarget {
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
