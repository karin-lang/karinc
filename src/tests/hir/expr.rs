use crate::{*, hir::*};
use crate::data::{ast::*, token::*};
use crate::data::hir::expr::*;

#[test]
fn lowers_function_expressions() {
    let node = AstNode::new(
        "fn_exprs".to_string(),
        vec![
            AstChild::leaf(
                "number".to_string(),
                Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
            ),
        ],
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_function_expressions(&node),
        vec![
            HirExpression::Number(
                HirNumberLiteral {
                    value: "0".to_string(),
                },
            ),
        ],
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn increments_local_code_in_function_expressions() {
    let node = AstNode::new(
        "fn_exprs".to_string(),
        vec![
            AstChild::node(
                "var_dec".to_string(),
                vec![
                    AstChild::leaf(
                        "id".to_string(),
                        Token::new(TokenKind::Id("id1".to_string()), 0, 1),
                    ),
                ],
            ),
            AstChild::node(
                "var_dec".to_string(),
                vec![
                    AstChild::leaf(
                        "id".to_string(),
                        Token::new(TokenKind::Id("id2".to_string()), 1, 1),
                    ),
                ],
            ),
        ],
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_function_expressions(&node),
        vec![
            HirExpression::VariableDeclaration(
                HirVariableDeclaration {
                    symbol: hir_symbol!(["id1"], 0),
                    initial_expr: None,
                },
            ),
            HirExpression::VariableDeclaration(
                HirVariableDeclaration {
                    symbol: hir_symbol!(["id2"], 1),
                    initial_expr: None,
                },
            ),
        ],
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_any_expression() {
    let child = AstChild::leaf(
        "number".to_string(),
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_expression(&child),
        Some(
            HirExpression::Number(
                HirNumberLiteral {
                    value: "0".to_string(),
                },
            ),
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_number_literal() {
    let leaf = AstLeaf::new(
        "number".to_string(),
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_number_literal(&leaf),
        Some(
            HirNumberLiteral {
                value: "0".to_string(),
            },
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_id_or_path() {
    let node = AstNode::new(
        "id_or_path".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Id("segment1".to_string()), 0, 1),
            ),
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Id("segment2".to_string()), 1, 1),
            ),
        ],
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_id_or_path(&node),
        vec![
            "segment1".to_string(),
            "segment2".to_string(),
        ],
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_variable_declaration() {
    let node = AstNode::new(
        "var_dec".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Id("id".to_string()), 0, 1),
            ),
        ],
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_variable_declaration(&node),
        Some(
            HirVariableDeclaration {
                symbol: hir_symbol!(["id"], 0),
                initial_expr: None,
            },
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_variable_declaration_with_initializer() {
    let node = AstNode::new(
        "var_dec".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Id("id".to_string()), 0, 1),
            ),
            AstChild::node(
                "var_dec_init".to_string(),
                vec![
                    AstChild::leaf(
                        "number".to_string(),
                        Token::new(TokenKind::Number(NumberToken("0".to_string())), 1, 1),
                    ),
                ],
            ),
        ],
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_variable_declaration(&node),
        Some(
            HirVariableDeclaration {
                symbol: hir_symbol!(["id"], 0),
                initial_expr: Some(
                    Box::new(
                        HirExpression::Number(HirNumberLiteral { value: "0".to_string() }),
                    ),
                ),
            },
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_function_call() {
    let node = AstNode::new(
        "fn_call".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Id("f".to_string()), 0, 0),
            ),
        ],
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_function_call(&node),
        Some(
            HirFunctionCall {
                symbol: hir_symbol!(["f"], 0),
                args: Vec::new(),
            },
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_function_call_with_args() {
    let node = AstNode::new(
        "fn_call".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Id("f".to_string()), 0, 0),
            ),
            AstChild::node(
                "actual_fn_args".to_string(),
                vec![
                    AstChild::leaf(
                        "number".to_string(),
                        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
                    ),
                    AstChild::leaf(
                        "number".to_string(),
                        Token::new(TokenKind::Number(NumberToken("1".to_string())), 0, 0),
                    ),
                ],
            ),
        ],
        
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_function_call(&node),
        Some(
            HirFunctionCall {
                symbol: hir_symbol!(["f"], 0),
                args: vec![
                    HirActualFunctionArgument::Expression(
                        HirExpression::Number(
                            HirNumberLiteral {
                                value: "0".to_string(),
                            }
                        ),
                    ),
                    HirActualFunctionArgument::Expression(
                        HirExpression::Number(
                            HirNumberLiteral {
                                value: "1".to_string(),
                            }
                        ),
                    ),
                ],
            },
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_actual_function_args() {
    let node = AstNode::new(
        "actual_fn_args".to_string(),
        vec![
            AstChild::leaf(
                "number".to_string(),
                Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
            ),
            AstChild::leaf(
                "number".to_string(),
                Token::new(TokenKind::Number(NumberToken("1".to_string())), 0, 0),
            ),
        ],
    );
    let mut lowering = HirLowering::new_l1_context();

    assert_eq!(
        lowering.lower_actual_function_args(&node),
        vec![
            HirActualFunctionArgument::Expression(
                HirExpression::Number(
                    HirNumberLiteral {
                        value: "0".to_string(),
                    },
                ),
            ),
            HirActualFunctionArgument::Expression(
                HirExpression::Number(
                    HirNumberLiteral {
                        value: "1".to_string(),
                    },
                ),
            ),
        ],
    );
    assert_eq!(lowering.logs, Vec::new());
}
