use crate::*;
use crate::new::{data::{ast::*, token::*}, parser::*};

#[test]
fn matches_all_elements_in_sequence() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 2, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        seq!(
            name: "seq";
            input: *input_iter;
            parser.parse_any_id(input_iter);
            parser.parse_any_symbol(input_iter);
        ),
        ParserResult::Matched(
            AstChild::node(
                "seq".to_string(),
                Vec::new(),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn specifies_leaf_to_ast_in_sequence() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 2, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        seq!(
            name: "seq";
            input: *input_iter;
            parser.parse_any_id(input_iter) => true;
            parser.parse_any_symbol(input_iter) => false;
        ),
        ParserResult::Matched(
            AstChild::node(
                "seq".to_string(),
                vec![AstChild::leaf(Token::new(TokenKind::Identifier("id".to_string()), 0, 2))],
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn does_not_match_element_in_sequence() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        seq!(
            name: "seq";
            input: *input_iter;
            parser.parse_any_id(input_iter);
            parser.parse_any_symbol(input_iter);
        ),
        ParserResult::Unmatched,
    );
    assert!(input_iter.next().is_some());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn choices_any_element() {
    let input = vec![
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        choice!(
            input: *input_iter;
            parser.parse_any_symbol(input_iter);
        ),
        ParserResult::Matched(AstChild::leaf(Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1))),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn does_not_choice_when_unmatched() {
    let input = vec![
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        choice!(
            input: *input_iter;
            parser.parse_any_id(input_iter);
        ),
        ParserResult::Unmatched,
    );
    assert!(input_iter.next().is_some());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_id_token() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_any_id(input_iter),
        ParserResult::Matched(AstChild::leaf(Token::new(TokenKind::Identifier("id".to_string()), 0, 2))),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn does_not_match_non_id_token() {
    let input = vec![
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(parser.parse_any_id(input_iter), ParserResult::Unmatched);
    assert!(input_iter.next().is_some());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_symbol_token() {
    let input = vec![
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_any_symbol(input_iter),
        ParserResult::Matched(AstChild::leaf(Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1))),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn does_not_match_non_symbol_token() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(parser.parse_any_symbol(input_iter), ParserResult::Unmatched);
    assert!(input_iter.next().is_some());
    assert_eq!(parser.logs, Vec::new());
}
