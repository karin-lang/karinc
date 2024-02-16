#[cfg(test)]
mod expr;
#[cfg(test)]
mod item;

use crate::*;
use crate::{data::{ast::*, token::*}, parser::*};

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
            parser.parse_any_id(input_iter) => Visible;
            parser.parse_any_symbol(input_iter) => Hidden;
        ),
        ParserResult::Matched(
            Some(
                AstChild::node(
                    "seq".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
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
fn does_not_match_element_in_sequence() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
        Token::new(TokenKind::Identifier("id".to_string()), 2, 2),
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
fn expands_node_in_sequence() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
        Token::new(TokenKind::Identifier("id".to_string()), 2, 2),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        seq!(
            name: "seq";
            input: *input_iter;
            seq!(
                name: "";
                input: *input_iter;
                parser.parse_any_id(input_iter) => Visible;
                parser.parse_any_id(input_iter) => Visible;
            ) => Expanded;
        ),
        ParserResult::Matched(
            Some(
                AstChild::node(
                    "seq".to_string(),
                    vec![
                        AstChild::leaf("id".to_string(), Token::new(TokenKind::Identifier("id".to_string()), 0, 2)),
                        AstChild::leaf("id".to_string(), Token::new(TokenKind::Identifier("id".to_string()), 2, 2)),
                    ],
                ),
            ),
        ),
    );
    assert!(input_iter.next().is_none());
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
            Some(
                AstChild::leaf(
                    "symbol".to_string(),
                    Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
                ),
            ),
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
fn keeps_matched_when_optional_element_matched() {
    assert_eq!(
        optional!(ParserCombinatoryResult::Matched(Some(AstChild::Node(AstNode::default())))),
        ParserResult::Matched(Some(AstChild::Node(AstNode::default()))),
    );
}

#[test]
fn makes_matched_when_optional_element_unmatched() {
    assert_eq!(
        optional!(ParserCombinatoryResult::Unmatched),
        ParserResult::Matched(None),
    );
}

#[test]
fn matches_any_number() {
    let input = vec![
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 1),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_any_number(input_iter),
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
fn matches_any_id_token() {
    let input = vec![
        Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
    ];
    let input_iter = &mut input.iter().peekable();
    let mut parser = Parser::new();

    assert_eq!(
        parser.parse_any_id(input_iter),
        ParserResult::Matched(
            Some(
                AstChild::leaf(
                    "id".to_string(),
                    Token::new(TokenKind::Identifier("id".to_string()), 0, 2),
                ),
            ),
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
            Some(
                AstChild::leaf(
                    "keyword".to_string(),
                    Token::new(TokenKind::Keyword(KeywordToken::Public), 0, 3),
                ),
            ),
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
            Some(
                AstChild::leaf(
                    "symbol".to_string(),
                    Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
                ),
            ),
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
                AstChild::leaf(
                    "symbol".to_string(),
                    Token::new(TokenKind::Symbol(SymbolToken::Semicolon), 0, 1),
                ),
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

#[test]
fn matches_single_item() {
    let input = vec![
        Token::new(TokenKind::Keyword(KeywordToken::Function), 0, 0),
        Token::new(TokenKind::Identifier("f".to_string()), 0, 1),
        Token::new(TokenKind::Symbol(SymbolToken::OpenParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingParen), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::OpenCurlyBracket), 0, 0),
        Token::new(TokenKind::Symbol(SymbolToken::ClosingCurlyBracket), 0, 0),
    ];
    let parser = Parser::new();

    assert_eq!(
        parser.parse(&input),
        (
            ParserResult::Matched(
                Some(
                    Ast::new(
                        AstNode::new(
                            "root".to_string(),
                            vec![
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
                            ],
                        ),
                    ),
                ),
            ),
            Vec::new(),
        ),
    );
}
