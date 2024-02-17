use crate::data::{ast::*, token::*};

#[test]
fn normalizes_ast() {
    let ast = Ast::new(
        AstNode::new(
            "root".to_string(),
            vec![
                AstChild::node(
                    "".to_string(),
                    vec![
                        AstChild::leaf(
                            "leaf".to_string(),
                            Token::new(TokenKind::Identifier("id".to_string()), 0, 0),
                        ),
                    ],
                ),
            ],
        ),
    );

    assert_eq!(
        ast.normalize(),
        Ast::new(
            AstNode::new(
                "root".to_string(),
                vec![
                    AstChild::leaf(
                        "leaf".to_string(),
                        Token::new(TokenKind::Identifier("id".to_string()), 0, 0),
                    ),
                ],
            ),
        ),
    );
}

#[test]
fn normalizes_node_children() {
    let node = AstNode::new(
        "parent".to_string(),
        vec![
            AstChild::node(
                "".to_string(),
                vec![
                    AstChild::leaf(
                        "leaf".to_string(),
                        Token::new(TokenKind::Identifier("id".to_string()), 0, 0),
                    ),
                ],
            ),
        ],
    );

    assert_eq!(
        node.normalize(),
        AstNode::new(
            "parent".to_string(),
            vec![
                AstChild::leaf(
                    "leaf".to_string(),
                    Token::new(TokenKind::Identifier("id".to_string()), 0, 0),
                ),
            ],
        ),
    );
}

#[test]
fn does_not_normalize_node_with_id() {
    let node = AstNode::new(
        "parent".to_string(),
        vec![
            AstChild::node(
                "WONT_BE_NORMALIZED".to_string(),
                vec![
                    AstChild::leaf(
                        "leaf".to_string(),
                        Token::new(TokenKind::Identifier("id".to_string()), 0, 0),
                    ),
                ],
            ),
        ],
    );

    assert_eq!(
        node.normalize(),
        AstNode::new(
            "parent".to_string(),
            vec![
                AstChild::node(
                    "WONT_BE_NORMALIZED".to_string(),
                    vec![
                        AstChild::leaf(
                            "leaf".to_string(),
                            Token::new(TokenKind::Identifier("id".to_string()), 0, 0),
                        ),
                    ],
                ),
            ],
        ),
    );
}
