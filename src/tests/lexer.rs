use crate::parser::ast;
use crate::{id_token, keyword_token, literal_token, prim_type_token, token};
use crate::lexer::tokenize::{Lexer, LexerLog};
use crate::lexer::token::*;

#[test]
fn skips_whitespaces() {
    let input = &mut " \t\n".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, Vec::new());
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn increments_token_position() {
    let input = &mut ";;\n;;".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![
        token!(Semicolon, 0, 0, 1),
        token!(Semicolon, 0, 1, 1),
        token!(Semicolon, 1, 0, 1),
        token!(Semicolon, 1, 1, 1),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_id() {
    let input = &mut "aA_".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("aA_", 0, 0, 3)]);
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_id_followed_by_numeric() {
    let input = &mut "a0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("a0", 0, 0, 2)]);
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_keyword() {
    let input = &mut "pub".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![keyword_token!(Pub, 0, 0, 3)]);
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_prim_type() {
    let input = &mut "usize".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![prim_type_token!(Usize, 0, 0, 5)]);
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_symbols() {
    // note: TokenKind にシンボルを追加する時はこのテストに記号を追加してください。
    let input = &mut "}):,.::={(;".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(
        tokens,
        vec![
            token!(ClosingCurlyBracket, 0, 0, 1),
            token!(ClosingParen, 0, 1, 1),
            token!(Colon, 0, 2, 1),
            token!(Comma, 0, 3, 1),
            token!(Dot, 0, 4, 1),
            token!(DoubleColon, 0, 5, 2),
            token!(Equal, 0, 7, 1),
            token!(OpenCurlyBracket, 0, 8, 1),
            token!(OpenParen, 0, 9, 1),
            token!(Semicolon, 0, 10, 1),
        ],
    );
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_multiple_character_symbol() {
    let input = &mut "::".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![token!(DoubleColon, 0, 0, 2)]);
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_int_literal_with_single_digit() {
    let input = &mut "0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_int_literal_with_multiple_digits() {
    let input = &mut "0aA_".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_base_of_int_literal() {
    let input = &mut "0b 0b0 0o0 0x0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_int_literal_with_type_suffix() {
    let input = &mut "0usize".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn expects_type_suffix_for_invalid_suffix_of_int_literal() {
    let input = &mut "0suffix".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
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
    assert!(input.peek().is_none());
    assert_eq!(
        logs,
        vec![LexerLog::ExpectedTypeSuffix { span: Span::new(0, 0, 7) }],
    );
}

#[test]
fn tokenizes_float_literal() {
    let input = &mut "0.0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn allows_fraction_digits_of_zero_len() {
    let input = &mut "0.".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_comma_after_fraction_digits_as_symbol() {
    let input = &mut "0.0.".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn tokenizes_base_of_float_literal() {
    let input = &mut "0b0.0 0o0.0 0x0.0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

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
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn consumes_unknown_char() {
    let input = &mut "\0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![token!(Unknown, 0, 0, 1)]);
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}

#[test]
fn ignores_continuous_unknown_chars() {
    let input = &mut "\0\0\0\n\0\0\0;\0\0\0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(
        tokens,
        vec![
            token!(Unknown, 0, 0, 3),
            token!(Unknown, 1, 0, 3),
            token!(Semicolon, 1, 3, 1),
            token!(Unknown, 1, 4, 3),
        ],
    );
    assert!(input.peek().is_none());
    assert_eq!(logs, Vec::new());
}
