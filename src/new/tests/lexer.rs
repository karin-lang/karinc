use crate::new::{data::token::{Token, TokenKind}, lexer::Lexer};

#[test]
fn tokenize_alphanumerics() {
    let input = "id";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![Token::new(TokenKind::Identifier("id".to_string()), 0, 2)]);
    assert_eq!(input_chars.peek(), None);
    assert_eq!(logs, Vec::new());
}

#[test]
fn does_not_match_non_alphanumerics() {
    let mut lexer = Lexer::new();
    let input = ";";
    let input_chars = &mut input.char_indices().peekable();

    assert_eq!(lexer.tokenize_alphanumerics(input_chars), None);
    assert_eq!(input_chars.peek(), Some(&(0, ';')));
    assert_eq!(lexer.logs, Vec::new());
}

#[test]
fn does_not_match_alphanumerics_of_zero_len() {
    let mut lexer = Lexer::new();
    let input = "";
    let input_chars = &mut input.char_indices().peekable();

    assert_eq!(lexer.tokenize_alphanumerics(input_chars), None);
    assert_eq!(input_chars.peek(), None);
    assert_eq!(lexer.logs, Vec::new());
}

#[test]
fn matches_single_alphanumeric() {
    let mut lexer = Lexer::new();
    let input = "0";
    let input_chars = &mut input.char_indices().peekable();

    assert_eq!(lexer.tokenize_alphanumerics(input_chars), Some("0".to_string()));
    assert_eq!(input_chars.peek(), None);
    assert_eq!(lexer.logs, Vec::new());
}

#[test]
fn matches_multiple_alphanumerics() {
    let mut lexer = Lexer::new();
    let input = "000";
    let input_chars = &mut input.char_indices().peekable();

    assert_eq!(lexer.tokenize_alphanumerics(input_chars), Some("000".to_string()));
    assert_eq!(input_chars.peek(), None);
    assert_eq!(lexer.logs, Vec::new());
}

#[test]
fn matches_all_kinds_of_alphanumerics() {
    let mut lexer = Lexer::new();
    let input = "0aA_";
    let input_chars = &mut input.char_indices().peekable();

    assert_eq!(lexer.tokenize_alphanumerics(input_chars), Some("0aA_".to_string()));
    assert_eq!(input_chars.peek(), None);
    assert_eq!(lexer.logs, Vec::new());
}
