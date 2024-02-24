use super::*;
use crate::data::{ast::*, token::*};

impl HirLowering {
    pub fn lower_function_expressions(&mut self, node: &AstNode) -> Vec<HirExpression> {
        let mut exprs = Vec::new();
        let local_code_generator = &mut HirLocalCodeGenerator::new();

        for expr_child in &node.children {
            let new_expr = self.lower_expression(expr_child, local_code_generator);

            if let Some(v) = new_expr {
                exprs.push(v);
            }
        }

        exprs
    }

    pub fn lower_expression(&mut self, child: &AstChild, local_code_generator: &mut HirLocalCodeGenerator) -> Option<HirExpression> {
        match child.get_name() {
            "number" => self.lower_number_literal(child.expect_leaf()).map(|v| HirExpression::Number(v)),
            "id_or_path" => {
                let segments = self.lower_id_or_path(child.expect_node());

                let value = HirRefIdOrPath {
                    segments,
                    name_resolution_status: HirNameResolutionStatus::Unresolved,
                };

                Some(HirExpression::NameResolutionTarget(value))
            },
            "var_dec" => self.lower_variable_declaration(child.expect_node(), local_code_generator).map(|v| HirExpression::VariableDeclaration(v)),
            "fn_call" => self.lower_function_call(child.expect_node(), local_code_generator).map(|v| HirExpression::FunctionCall(v)),
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

    pub fn lower_id_or_path(&mut self, node: &AstNode) -> Vec<String> {
        let mut segments = Vec::new();

        for each_segment in &node.children {
            if let TokenKind::Id(id) = &each_segment.expect_leaf().value.kind {
                segments.push(id.clone());
            } else {
                panic!("expected id token in id_or_path node");
            }
        }

        segments
    }

    pub fn lower_variable_declaration(&mut self, node: &AstNode, local_code_generator: &mut HirLocalCodeGenerator) -> Option<HirVariableDeclaration> {
        let id_leaf = node.find("id").unwrap().expect_leaf();
        let id = HirDefId(id_leaf.value.kind.expect_id().clone());
        let code = local_code_generator.generate();

        let initial_expr = node.find("var_dec_init").and_then(|init_child| {
            self
                .lower_expression(init_child.expect_node().get(0).unwrap(), local_code_generator)
                .map(|v| Box::new(v))
        });

        let variable_declaration = HirVariableDeclaration {
            code: HirDefLocalCode { id, code },
            initial_expr,
        };
        Some(variable_declaration)
    }

    pub fn lower_function_call(&mut self, node: &AstNode, local_code_generator: &mut HirLocalCodeGenerator) -> Option<HirFunctionCall> {
        let id_leaf = node.find("id").unwrap().expect_leaf();

        let id = HirRefIdOrPath {
            segments: vec![id_leaf.value.kind.expect_id().clone()],
            name_resolution_status: HirNameResolutionStatus::Unresolved,
        };

        let args = node
            .find("actual_fn_args")
            .map(|v| self.lower_actual_function_args(v.expect_node(), local_code_generator))
            .unwrap_or(Vec::new());

        let function_call = HirFunctionCall { id, args };
        Some(function_call)
    }

    pub fn lower_actual_function_args(&mut self, node: &AstNode, local_code_generator: &mut HirLocalCodeGenerator) -> Vec<HirActualFunctionArgument> {
        let mut args = Vec::new();

        for each_child in &node.children {
            let new_arg = self.lower_expression(each_child, local_code_generator).map(|v| HirActualFunctionArgument::Expression(v));

            if let Some(v) = new_arg {
                args.push(v);
            }
        }

        args
    }
}
