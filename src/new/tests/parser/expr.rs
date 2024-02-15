use crate::new::{data::{ast::*, token::*}, parser::*};

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
                )
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}
