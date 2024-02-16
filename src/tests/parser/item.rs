use crate::{data::{ast::*, token::*}, parser::*};

#[test]
fn matches_any_item() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Function), 0, 0),
        Token::new(TokenKind::Identifier("f".to_string()), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::OpenParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::OpenCurlyBracket), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingCurlyBracket), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_item(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_dec".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Identifier("f".to_string()), 0, 1),
                        ),
                        AstChild::node(
                            "fn_exprs".to_string(),
                            Vec::new(),
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
fn matches_accessibility() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Public), 0, 3),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_accessibility(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::leaf(
                    "accessibility".to_string(),
                    Token::new(TokenKind::Keyword(KeywordToken::Public), 0, 3),
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_function_declaration() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Function), 0, 0),
        Token::new(TokenKind::Identifier("f".to_string()), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::OpenParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::OpenCurlyBracket), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingCurlyBracket), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_declaration(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_dec".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Identifier("f".to_string()), 0, 1),
                        ),
                        AstChild::node(
                            "fn_exprs".to_string(),
                            Vec::new(),
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
fn matches_function_declaration_accessibility_optionally() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Public), 0, 1),
        Token::new(TokenKind::Keyword(KeywordToken::Function), 0, 0),
        Token::new(TokenKind::Identifier("f".to_string()), 1, 1),
        Token::new(TokenKind::Symbol(SymbolToken::OpenParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::OpenCurlyBracket), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingCurlyBracket), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_declaration(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_dec".to_string(),
                    vec![
                        AstChild::leaf(
                            "accessibility".to_string(),
                            Token::new(TokenKind::Keyword(KeywordToken::Public), 0, 1),
                        ),
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Identifier("f".to_string()), 1, 1),
                        ),
                        AstChild::node(
                            "fn_exprs".to_string(),
                            Vec::new(),
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
fn matches_function_declaration_with_expression() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Function), 0, 0),
        Token::new(TokenKind::Identifier("f".to_string()), 1, 1),
        Token::new(TokenKind::Symbol(SymbolToken::OpenParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::OpenCurlyBracket), 0, 0),
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingCurlyBracket), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_declaration(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_dec".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Identifier("f".to_string()), 1, 1),
                        ),
                        AstChild::node(
                            "fn_exprs".to_string(),
                            vec![
                                AstChild::node(
                                    "".to_string(),
                                    vec![
                                        AstChild::leaf(
                                            "number".to_string(),
                                            Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                                        ),
                                    ],
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
fn matches_function_exprs_of_zero_len() {
    let input = Vec::new();
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_exprs(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_exprs".to_string(),
                    Vec::new(),
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_single_function_expr() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_exprs(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_exprs".to_string(),
                    vec![
                        AstChild::node(
                            "".to_string(),
                            vec![
                                AstChild::leaf(
                                    "number".to_string(),
                                    Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
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
fn matches_multiple_function_exprs() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 0),
        Token::new(TokenKind::Number(NumberToken("1".to_string())), 1, 1),
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 0),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_function_exprs(input_iter),
        ParserCombinatoryResult::Matched(
            Some(
                AstChild::node(
                    "fn_exprs".to_string(),
                    vec![
                        AstChild::node(
                            "".to_string(),
                            vec![
                                AstChild::leaf(
                                    "number".to_string(),
                                    Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
                                ),
                            ],
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
