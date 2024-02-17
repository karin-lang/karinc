use crate::data::{ast::*, token::*};
use crate::data::hir::{expr::*, item::*};
use crate::hir::*;

#[test]
fn lowers_any_item() {
    let node = AstNode::new(
        "fn_dec".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Identifier("f".to_string()), 0, 0),
            ),
            AstChild::node(
                "fn_exprs".to_string(),
                Vec::new(),
            ),
        ],
    );
    let mut lowering = HirLowering::new();

    assert_eq!(
        lowering.lower_item(&node),
        Some(
            HirItem::FunctionDeclaration(
                HirFunctionDeclaration {
                    id: HirId("f".to_string()),
                    exprs: Vec::new(),
                },
            ),
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn fails_to_lower_unknown_item_node() {
    let node = AstNode::new(
        "UNKNOWN_NODE".to_string(),
        Vec::new(),
    );
    let mut lowering = HirLowering::new();

    assert_eq!(lowering.lower_item(&node), None);
    assert_eq!(
        lowering.logs,
        vec![
            HirLoweringLog::UnknownNodeId("UNKNOWN_NODE".to_string()),
        ],
    );
}

#[test]
fn lowers_function_declaration() {
    let node = AstNode::new(
        "fn_dec".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Identifier("f".to_string()), 0, 0),
            ),
            AstChild::node(
                "fn_exprs".to_string(),
                Vec::new(),
            ),
        ],
    );
    let mut lowering = HirLowering::new();

    assert_eq!(
        lowering.lower_function_declaration(&node),
        Some(
            HirFunctionDeclaration {
                id: HirId("f".to_string()),
                exprs: Vec::new(),
            },
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_function_declaration_with_expression() {
    let node = AstNode::new(
        "fn_dec".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Identifier("f".to_string()), 0, 0),
            ),
            AstChild::node(
                "fn_exprs".to_string(),
                vec![
                    AstChild::leaf(
                        "number".to_string(),
                        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
                    ),
                ],
            ),
        ],
    );
    let mut lowering = HirLowering::new();

    assert_eq!(
        lowering.lower_function_declaration(&node),
        Some(
            HirFunctionDeclaration {
                id: HirId("f".to_string()),
                exprs: vec![
                    HirExpression::Number(
                        HirNumberLiteral {
                            value: "0".to_string(),
                        },
                    ),
                ],
            },
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

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
    let mut lowering = HirLowering::new();

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
