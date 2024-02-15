use std::vec;

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
            Some(
                AstChild::node(
                    "seq".to_string(),
                    Vec::new(),
                ),
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
            Some(
                AstChild::node(
                    "seq".to_string(),
                    vec![AstChild::leaf(Token::new(TokenKind::Identifier("id".to_string()), 0, 2))],
                ),
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
        ParserResult::Matched(
            Some(AstChild::leaf(Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1))),
        ),
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
                        AstChild::leaf(Token::new(TokenKind::Identifier("f".to_string()), 0, 1)),
                    ],
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn keeps_matched_when_optional_element_matched() {
    assert_eq!(
        Parser::optional(
            ParserCombinatoryResult::Matched(Some(AstChild::Node(AstNode::default()))),
        ),
        ParserResult::Matched(Some(AstChild::Node(AstNode::default()))),
    );
}

#[test]
fn makes_matched_when_optional_element_unmatched() {
    assert_eq!(
        Parser::optional(ParserCombinatoryResult::Unmatched),
        ParserResult::Matched(None),
    );
}

#[test]
fn matches_any_id_token() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_any_id(input_iter),
        ParserResult::Matched(
            Some(AstChild::leaf(Token::new(TokenKind::Identifier("id".to_string()), 0, 2))),
        ),
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
fn matches_keyword_token_completely() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Public), 0, 3),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_keyword(input_iter, KeywordToken::Public),
        ParserResult::Matched(
            Some(AstChild::leaf(Token::new(TokenKind::Keyword(KeywordToken::Public), 0, 3))),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn does_not_match_wrong_keyword_token() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Function), 0, 2),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(parser.parse_keyword(input_iter, KeywordToken::Public), ParserResult::Unmatched);
    assert!(input_iter.next().is_some());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn matches_any_symbol_token() {
    let input = vec![
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_any_symbol(input_iter),
        ParserResult::Matched(
            Some(AstChild::leaf(Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1))),
        ),
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

#[test]
fn matches_symbol_token_completely() {
    let input = vec![
        Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_symbol(input_iter, SymbolToken::Semicolon),
        ParserResult::Matched(
            Some(
                AstChild::leaf(Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1)),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
    assert_eq!(parser.logs, Vec::new());
}

#[test]
fn does_not_match_wrong_symbol_token() {
    let input = vec![
        Token::new(TokenKind::Symbol(SymbolToken::Colon), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(parser.parse_symbol(input_iter, SymbolToken::Semicolon), ParserResult::Unmatched);
    assert!(input_iter.next().is_some());
    assert_eq!(parser.logs, Vec::new());
}
