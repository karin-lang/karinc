use crate::lexer::{token::*, Lexer};

#[test]
fn generates_empty_at_eof() {
    assert_eq!(
        Lexer::tokenize(""),
        (Vec::new(), Vec::new()),
    );
}

#[test]
fn generates_single_token() {
    assert_eq!(
        Lexer::tokenize("("),
        (
            vec![Token::Symbol(SymbolToken::OpenParen)],
            Vec::new(),
        ),
    );
}

#[test]
fn generates_multiple_tokens() {
    assert_eq!(
        Lexer::tokenize("fn(id)"),
        (
            vec![
                Token::Keyword(KeywordToken::Function),
                Token::Symbol(SymbolToken::OpenParen),
                Token::Identifier("id".to_string()),
                Token::Symbol(SymbolToken::ClosingParen),
            ],
            Vec::new(),
        ),
    );
}

#[test]
fn generates_single_character_identifier() {
    assert_eq!(
        Lexer::tokenize("a"),
        (
            vec![Token::Identifier("a".to_string())],
            Vec::new(),
        ),
    );
}

#[test]
fn generates_multiple_character_identifier() {
    assert_eq!(
        Lexer::tokenize("aA"),
        (
            vec![Token::Identifier("aA".to_string())],
            Vec::new(),
        ),
    );
}

#[test]
fn does_not_match_identifier_starts_with_number() {
    assert_ne!(
        Lexer::tokenize("0").0,
        vec![Token::Identifier("0".to_string())],
    );
}

#[test]
fn matches_identifier_containing_number_at_second() {
    assert_eq!(
        Lexer::tokenize("a0"),
        (
            vec![Token::Identifier("a0".to_string())],
            Vec::new(),
        ),
    );
}

#[test]
fn matches_identifier_starts_and_ends_with_underbar() {
    assert_eq!(
        Lexer::tokenize("__"),
        (
            vec![Token::Identifier("__".to_string())],
            Vec::new(),
        ),
    );
}

#[test]
fn generates_symbol_token() {
    assert_eq!(
        Lexer::tokenize("("),
        (
            vec![Token::Symbol(SymbolToken::OpenParen)],
            Vec::new(),
        ),
    );
}
