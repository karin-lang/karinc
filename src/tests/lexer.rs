use crate::parser::ast;
use crate::{id_token, keyword_token, literal_token, token};
use crate::lexer::tokenize::{Lexer, LexerLog};
use crate::lexer::token::*;

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
fn tokenizes_id() {
    let input = "aA_";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![id_token!("aA_", 0, 0, 3)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_id_followed_by_numeric() {
    let input = "a0";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![id_token!("a0", 0, 0, 2)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_keyword() {
    let input = "pub";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![keyword_token!(Pub, 0, 0, 3)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_symbol() {
    let input = ";";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![token!(Semicolon, 0, 0, 1)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_multiple_character_symbol() {
    let input = "::";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![token!(DoubleColon, 0, 0, 2)]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_int_literal_with_single_digit() {
    let input = "0";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Int {
                base: Base::Dec,
                int_digits: "0".to_string(),
                r#type: None,
            },
            0, 0, 1,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_int_literal_with_multiple_digits() {
    let input = "0aA_";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Int {
                base: Base::Dec,
                int_digits: "0aA_".to_string(),
                r#type: None,
            },
            0, 0, 4,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_base_of_int_literal() {
    let input = "0b 0b0 0o0 0x0";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Int {
                base: Base::Bin,
                int_digits: "".to_string(),
                r#type: None,
            },
            0, 0, 2,
        ),
        literal_token!(
            Literal::Int {
                base: Base::Bin,
                int_digits: "0".to_string(),
                r#type: None,
            },
            0, 3, 3,
        ),
        literal_token!(
            Literal::Int {
                base: Base::Oct,
                int_digits: "0".to_string(),
                r#type: None,
            },
            0, 7, 3,
        ),
        literal_token!(
            Literal::Int {
                base: Base::Hex,
                int_digits: "0".to_string(),
                r#type: None,
            },
            0, 11, 3,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_int_literal_with_type_suffix() {
    let input = "0usize";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Int {
                base: Base::Dec,
                int_digits: "0".to_string(),
                r#type: Some(ast::PrimType::Usize),
            },
            0, 0, 6,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn expects_type_suffix_for_invalid_suffix_of_int_literal() {
    let input = "0suffix";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Int {
                base: Base::Dec,
                int_digits: "0".to_string(),
                r#type: None,
            },
            0, 0, 7,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(
        logs,
        vec![LexerLog::ExpectedTypeSuffix { span: Span::new(0, 0, 7) }],
    );
}

#[test]
fn tokenizes_float_literal() {
    let input = "0.0";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                base: Base::Dec,
                int_digits: "0".to_string(),
                fraction_digits: "0".to_string(),
                r#type: None,
            },
            0, 0, 3,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn allows_fraction_digits_of_zero_len() {
    let input = "0.";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                base: Base::Dec,
                int_digits: "0".to_string(),
                fraction_digits: "".to_string(),
                r#type: None,
            },
            0, 0, 2,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_comma_after_fraction_digits_as_symbol() {
    let input = "0.0.";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                base: Base::Dec,
                int_digits: "0".to_string(),
                fraction_digits: "0".to_string(),
                r#type: None,
            },
            0, 0, 3,
        ),
        token!(Dot, 0, 3, 1),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_base_of_float_literal() {
    let input = "0b0.0 0o0.0 0x0.0";
    let input_chars = &mut input.char_indices().peekable();
    let (tokens, logs) = Lexer::new().tokenize_(input_chars);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                base: Base::Bin,
                int_digits: "0".to_string(),
                fraction_digits: "0".to_string(),
                r#type: None,
            },
            0, 0, 5,
        ),
        literal_token!(
            Literal::Float {
                base: Base::Oct,
                int_digits: "0".to_string(),
                fraction_digits: "0".to_string(),
                r#type: None,
            },
            0, 6, 5,
        ),
        literal_token!(
            Literal::Float {
                base: Base::Hex,
                int_digits: "0".to_string(),
                fraction_digits: "0".to_string(),
                r#type: None,
            },
            0, 12, 5,
        ),
    ]);
    assert!(input_chars.peek().is_none());
    assert_eq!(logs, Vec::new());
}
