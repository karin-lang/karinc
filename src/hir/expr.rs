use super::*;
use crate::data::{ast::*, token::*};

impl HirLowering {
    pub fn lower_function_body(&mut self, node: &AstNode) -> Vec<HirExpression> {
        self.enter_function_context();

        let mut exprs = Vec::new();

        for expr_child in &node.children {
            let new_expr = self.lower_expression(expr_child);

            if let Some(v) = new_expr {
                exprs.push(v);
            }
        }

        self.exit_function_context();

        exprs
    }

    pub fn lower_expression(&mut self, child: &AstChild) -> Option<HirExpression> {
        match child.get_name() {
            "number" => self.lower_number_literal(child.expect_leaf()).map(|v| HirExpression::Number(v)),
            "id_or_path" => {
                let symbol = HirSymbolAccessor {
                    index: self.generate_symbol_index(),
                    kind: self.lower_path_or_member_access_chain(child.expect_node()),
                };
                Some(HirExpression::Symbol(symbol))
            },
            "var_dec" => self.lower_variable_declaration(child.expect_node()).map(|v| HirExpression::VariableDeclaration(v)),
            "fn_call" => self.lower_function_call(child.expect_node()).map(|v| HirExpression::FunctionCall(v)),
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

    pub fn lower_path_or_member_access_chain(&mut self, node: &AstNode) -> HirSymbolAccessorKind {
        let mut segments = Vec::new();

        for each_segment in &node.children {
            if let TokenKind::Id(id) = &each_segment.expect_leaf().value.kind {
                segments.push(id.clone());
            } else {
                panic!("expected id token in id_or_path node");
            }
        }

        // todo: segments が empty だった場合のエラー処理を実装する

        if segments.len() == 1 {
            HirSymbolAccessorKind::SingleSegment(segments.get(0).unwrap().clone())
        } else {
            HirSymbolAccessorKind::MultipleSegments(
                HirPath { segments },
                HirMemberAccessChain { segments: Vec::new() },
            )
        }
    }

    pub fn lower_variable_declaration(&mut self, node: &AstNode) -> Option<HirVariableDeclaration> {
        let id_leaf = node.find("id").unwrap().expect_leaf();
        let id = id_leaf.value.kind.expect_id().clone();
        let code = self.generate_symbol_code();

        let initial_expr = node.find("var_dec_init").and_then(|init_child| {
            self
                .lower_expression(init_child.expect_node().get(0).unwrap())
                .map(|v| Box::new(v))
        });

        let variable_declaration = HirVariableDeclaration {
            symbol: HirLocalSymbol { id, code },
            initial_expr,
        };

        Some(variable_declaration)
    }

    pub fn lower_function_call(&mut self, node: &AstNode) -> Option<HirFunctionCall> {
        let id_leaf = node.find("id").unwrap().expect_leaf();
        let id = id_leaf.value.kind.expect_id().clone();
        let index = self.generate_symbol_index();
        let symbol = HirSymbolAccessor { index, kind: HirSymbolAccessorKind::SingleSegment(id) };

        let args = node
            .find("actual_fn_args")
            .map(|v| self.lower_actual_function_args(v.expect_node()))
            .unwrap_or(Vec::new());

        let function_call = HirFunctionCall { symbol, args };
        Some(function_call)
    }

    pub fn lower_actual_function_args(&mut self, node: &AstNode) -> Vec<HirActualFunctionArgument> {
        let mut args = Vec::new();

        for each_child in &node.children {
            let new_arg = self
                .lower_expression(each_child)
                .map(|v| HirActualFunctionArgument::Expression(v));

            if let Some(v) = new_arg {
                args.push(v);
            }
        }

        args
    }
}
