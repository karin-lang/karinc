use crate::{data::{ast::*, token::*}, parser::*};

#[test]
fn matches_any_expression() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_expr(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::leaf(
                    "number".to_string(),
                    Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_number_literal() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_number_literal(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::leaf(
                    "number".to_string(),
                    Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_function_call() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::OpenParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingParen), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_call(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_call".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Identifier("id".to_string()), 0, 1),
                        ),
                    ],
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_function_call_with_actual_argument() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::OpenParen), 0, 0),
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 1, 1),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingParen), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_call(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_call".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Identifier("id".to_string()), 0, 1),
                        ),
                        AstChild::node(
                            "actual_fn_args".to_string(),
                            vec![
                                AstChild::leaf(
                                    "number".to_string(),
                                    Token::new(TokenKind::Number(NumberToken("0".to_string())), 1, 1),
                                ),
                            ],
                        ),
                    ],
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_actual_function_arguments_of_zero_len() {
    let input = Vec::new();
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_actual_function_arguments(input_iter),
        ParserCombinatoryResult::Matched(None),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_single_actual_function_argument() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_actual_function_arguments(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "actual_fn_args".to_string(),
                    vec![
                        AstChild::leaf(
                            "number".to_string(),
                            Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                        ),
                    ],
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn allows_comma_after_actual_function_argument() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Comma), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_actual_function_arguments(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "actual_fn_args".to_string(),
                    vec![
                        AstChild::leaf(
                            "number".to_string(),
                            Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                        ),
                    ],
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_two_actual_function_arguments() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Comma), 0, 0),
        Token::new(TokenKind::Number(NumberToken("1".to_string())), 1, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_actual_function_arguments(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "actual_fn_args".to_string(),
                    vec![
                        AstChild::leaf(
                            "number".to_string(),
                            Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                        ),
                        AstChild::node(
                            "".to_string(),
                            vec![
                                AstChild::leaf(
                                    "number".to_string(),
                                    Token::new(TokenKind::Number(NumberToken("1".to_string())), 1, 1),
                                ),
                            ],
                        ),
                    ],
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_three_actual_function_arguments() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Comma), 0, 0),
        Token::new(TokenKind::Number(NumberToken("1".to_string())), 1, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Comma), 0, 0),
        Token::new(TokenKind::Number(NumberToken("2".to_string())), 2, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_actual_function_arguments(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "actual_fn_args".to_string(),
                    vec![
                        AstChild::leaf(
                            "number".to_string(),
                            Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                        ),
                        AstChild::node(
                            "".to_string(),
                            vec![
                                AstChild::leaf(
                                    "number".to_string(),
                                    Token::new(TokenKind::Number(NumberToken("1".to_string())), 1, 1),
                                ),
                            ],
                        ),
                        AstChild::node(
                            "".to_string(),
                            vec![
                                AstChild::leaf(
                                    "number".to_string(),
                                    Token::new(TokenKind::Number(NumberToken("2".to_string())), 2, 1),
                                ),
                            ],
                        ),
                    ],
                )
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}
