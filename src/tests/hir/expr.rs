use crate::data::{ast::*, token::*};
use crate::data::hir::expr::*;
use crate::hir::*;

#[test]
fn lowers_any_expression() {
    let child = AstChild::leaf(
        "number".to_string(),
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
    );
    let mut lowering = HirLowering::new();

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
    let mut lowering = HirLowering::new();

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
fn lowers_function_call() {
    let node = AstNode::new(
        "fn_call".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Identifier("f".to_string()), 0, 0),
            ),
        ],
    );
    let mut lowering = HirLowering::new();

    assert_eq!(
        lowering.lower_function_call(&node),
        Some(
            HirFunctionCall {
                id: HirId("f".to_string()),
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
                Token::new(TokenKind::Identifier("f".to_string()), 0, 0),
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
    let mut lowering = HirLowering::new();

    assert_eq!(
        lowering.lower_function_call(&node),
        Some(
            HirFunctionCall {
                id: HirId("f".to_string()),
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
    let mut lowering = HirLowering::new();

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
