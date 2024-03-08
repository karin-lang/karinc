use crate::{id_token, keyword_token, literal_token, token};
use crate::lexer::tokenize::Lexer;
use crate::lexer::token::Literal;

#[test]
fn skips_whitespaces() {
    let input = " \t\n";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, Vec::new());
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn increments_token_position() {
    let input = ";;\n;;";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        token!(Semicolon, 0, 0, 1),
        token!(Semicolon, 0, 1, 1),
        token!(Semicolon, 1, 0, 1),
        token!(Semicolon, 1, 1, 1),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenize_id() {
    let input = "aA_";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![id_token!("aA_", 0, 0, 3)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenize_id_followed_by_numeric() {
    let input = "a0";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![id_token!("a0", 0, 0, 2)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenize_keyword() {
    let input = "pub";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![keyword_token!(Pub, 0, 0, 3)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenize_symbol() {
    let input = ";";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![token!(Semicolon, 0, 0, 1)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenize_multiple_character_symbol() {
    let input = "::";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![token!(DoubleColon, 0, 0, 2)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn matches_int_with_single_digit() {
    let input = "0";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Int { value: "0".to_string() }, 0, 0, 1),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn matches_int_multiple_digit() {
    let input = "000";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        literal_token!(Literal::Int { value: "000".to_string() }, 0, 0, 3),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}
