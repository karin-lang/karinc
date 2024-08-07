use crate::parser::ast;
use crate::{id_token, keyword_token, literal_token, prim_type_token, token};
use crate::lexer::log::LexerLog;
use crate::lexer::tokenize::Lexer;
use crate::lexer::token::*;

#[test]
fn skips_whitespaces() {
    let input = &mut " \t\r\n".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert!(tokens.is_empty());
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn increments_token_index() {
    let input = &mut ";;\n;;".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![
        token!(Semicolon, 0, 1),
        token!(Semicolon, 1, 1),
        token!(Semicolon, 3, 1),
        token!(Semicolon, 4, 1),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_id() {
    let input = &mut "aA_".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("aA_", 0, 3)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_id_with_single_char() {
    let input = &mut "a".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("a", 0, 1)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_id_followed_by_numeric() {
    let input = &mut "a0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("a0", 0, 2)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_keyword() {
    let input = &mut "pub".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![keyword_token!(Pub, 0, 3)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_id_starts_with_byte_prefix() {
    let input = &mut "bit".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("bit", 0, 3)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_id_starts_with_raw_prefix() {
    let input = &mut "regex".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("regex", 0, 5)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_id_starts_with_raw_byte_prefix() {
    let input = &mut "bruh".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![id_token!("bruh", 0, 4)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_prim_types() {
    let input = &mut "bool isize usize f32 char".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![
        prim_type_token!(Bool, 0, 4),
        prim_type_token!(Isize, 5, 5),
        prim_type_token!(Usize, 11, 5),
        prim_type_token!(F32, 17, 3),
        prim_type_token!(Char, 21, 4),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_symbols() {
    // note: TokenKind にシンボルを追加する時はこのテストに記号を追加してください。
    let input = &mut "}):,.::={(;".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(
        tokens,
        vec![
            token!(ClosingCurlyBracket, 0, 1),
            token!(ClosingParen, 1, 1),
            token!(Colon, 2, 1),
            token!(Comma, 3, 1),
            token!(Dot, 4, 1),
            token!(DoubleColon, 5, 2),
            token!(Equal, 7, 1),
            token!(OpenCurlyBracket, 8, 1),
            token!(OpenParen, 9, 1),
            token!(Semicolon, 10, 1),
        ],
    );
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_multiple_character_symbol() {
    let input = &mut "::".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![token!(DoubleColon, 0, 2)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_true_literal() {
    let input = &mut "true".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![literal_token!(Literal::Bool { value: true }, 0, 4)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_false_literal() {
    let input = &mut "false".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![literal_token!(Literal::Bool { value: false }, 0, 5)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
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
            0, 1,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
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
            0, 4,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
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
            0, 2,
        ),
        literal_token!(
            Literal::Int {
                base: Base::Bin,
                int_digits: "0".to_string(),
                r#type: None,
            },
            3, 3,
        ),
        literal_token!(
            Literal::Int {
                base: Base::Oct,
                int_digits: "0".to_string(),
                r#type: None,
            },
            7, 3,
        ),
        literal_token!(
            Literal::Int {
                base: Base::Hex,
                int_digits: "0".to_string(),
                r#type: None,
            },
            11, 3,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
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
            0, 6,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
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
            0, 7,
        ),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(
        logs,
        vec![LexerLog::ExpectedTypeSuffix { span: Span::new(0, 7) }],
    );
}

#[test]
fn tokenizes_float_literal() {
    let input = &mut "0.0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                digits: Some(
                    FloatDigits {
                        int: "0".to_string(),
                        fraction: "0".to_string(),
                    },
                ),
                r#type: None,
            },
            0, 3,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn allows_fraction_digits_of_zero_len() {
    let input = &mut "0.".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                digits: Some(
                    FloatDigits {
                        int: "0".to_string(),
                        fraction: "".to_string(),
                    },
                ),
                r#type: None,
            },
            0, 2,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_comma_after_fraction_digits_as_symbol() {
    let input = &mut "0.0.".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                digits: Some(
                    FloatDigits {
                        int: "0".to_string(),
                        fraction: "0".to_string(),
                    },
                ),
                r#type: None,
            },
            0, 3,
        ),
        token!(Dot, 3, 1),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn disallows_non_dec_digits_of_float_literal() {
    let input = &mut "0b0.0 0o0.0 0x0.0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Float {
                digits: None,
                r#type: None,
            },
            0, 5,
        ),
        literal_token!(
            Literal::Float {
                digits: None,
                r#type: None,
            },
            6, 5,
        ),
        literal_token!(
            Literal::Float {
                digits: None,
                r#type: None,
            },
            12, 5,
        ),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(
        logs,
        vec![
            LexerLog::ExpectedDecimalFloat { span: Span::new(0, 5) },
            LexerLog::ExpectedDecimalFloat { span: Span::new(6, 5) },
            LexerLog::ExpectedDecimalFloat { span: Span::new(12, 5) },
        ],
    );
}

#[test]
fn tokenizes_char_literal() {
    let input = &mut "'a'".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: Some('a') }, 0, 3),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn detects_empty_char_literal() {
    let input = &mut "''".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: None }, 0, 2),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(
        logs,
        vec![LexerLog::EmptyCharLiteral { span: Span::new(0, 2) }],
    );
}

#[test]
fn detects_too_long_char_literal() {
    let input: &mut crate::lexer::tokenize::LexerInput<'_> = &mut "'ab' 'abc'".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: None }, 0, 4),
        literal_token!(Literal::Char { value: None }, 5, 5),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(
        logs,
        vec![
            LexerLog::TooLongCharLiteral { span: Span::new(0, 4) },
            LexerLog::TooLongCharLiteral { span: Span::new(5, 5) },
        ],
    );
}

#[test]
fn tokenizes_escseq_in_char_literal() {
    let input = &mut r#"'\\' '\'' '\"' '\0' '\n' '\r' '\t'"#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: Some('\\') }, 0, 4),
        literal_token!(Literal::Char { value: Some('\'') }, 5, 4),
        literal_token!(Literal::Char { value: Some('\"') }, 10, 4),
        literal_token!(Literal::Char { value: Some('\0') }, 15, 4),
        literal_token!(Literal::Char { value: Some('\n') }, 20, 4),
        literal_token!(Literal::Char { value: Some('\r') }, 25, 4),
        literal_token!(Literal::Char { value: Some('\t') }, 30, 4),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn records_log_when_encountered_unknown_escseq_in_char_literal() {
    let input = &mut r"'\?'".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: None }, 0, 4),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![
        LexerLog::UnknownEscseq { span: Span::new(1, 2) },
        LexerLog::EmptyCharLiteral { span: Span::new(0, 4) },
    ]);
}

#[test]
fn records_log_at_eof_after_escseq_prefix_in_char_literal() {
    let input = &mut r"'\".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: None }, 0, 2),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![
        LexerLog::UnclosedCharLiteral { span: Span::new(0, 2) },
        LexerLog::EmptyCharLiteral { span: Span::new(0, 2) },
    ]);
}

#[test]
fn does_not_support_raw_char_literal() {
    let input = &mut "r' '".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        id_token!("r", 0, 1),
        literal_token!(Literal::Char { value: Some(' ') }, 1, 3),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_byte_char_literal() {
    let input = &mut "b' '".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::ByteChar { value: Some(' ') }, 0, 4),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn does_not_support_raw_byte_char_literal() {
    let input = &mut "brr' '".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        id_token!("brr", 0, 3),
        literal_token!(Literal::Char { value: Some(' ') }, 3, 3),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn detects_line_break_in_char_literal_and_skips_to_next_single_quot() {
    let input = &mut "'\nskipped\nskipped'tokenized".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: None }, 0, 1),
        id_token!("tokenized", 18, 9),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(
        logs,
        vec![
            LexerLog::LineBreakInCharLiteral { span: Span::new(0, 1) },
            LexerLog::EmptyCharLiteral { span: Span::new(0, 1) },
        ],
    );
}

#[test]
fn detects_line_break_in_char_literal_and_skips_to_eof() {
    let input = &mut "'\nskipped\nskipped".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: None }, 0, 1),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(
        logs,
        vec![
            LexerLog::LineBreakInCharLiteral { span: Span::new(0, 1) },
            LexerLog::EmptyCharLiteral { span: Span::new(0, 1) },
        ],
    );
}

#[test]
fn detects_unclosed_char_literal() {
    let input = &mut "'a".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(Literal::Char { value: Some('a') }, 0, 2),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![LexerLog::UnclosedCharLiteral { span: Span::new(0, 2) }]);
}

#[test]
fn tokenizes_empty_str_literal() {
    let input = &mut r#""""#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: String::new() },
            0, 2,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_str_literal_including_normal_char() {
    let input = &mut r#""abc""#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "abc".to_string() },
            0, 5,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_escseq_in_str_literal() {
    let input = &mut r#""\\\'\"\0\n\r\t""#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "\\\'\"\0\n\r\t".to_string() },
            0, 16,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn records_log_when_encountered_unknown_escseq_in_str_literal() {
    let input = &mut r#""abc\?def""#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "abcdef".to_string() },
            0, 10,
        ),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![LexerLog::UnknownEscseq { span: Span::new(4, 2) }]);
}

#[test]
fn records_log_at_eof_after_escseq_prefix_in_str_literal() {
    let input = &mut r#""abc\"#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "abc".to_string() },
            0, 5,
        ),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![LexerLog::UnclosedStrLiteral { span: Span::new(0, 5) }]);
}

#[test]
fn detects_line_break_after_escseq_prefix_in_str_literal() {
    let input = &mut "\"abc\\\n\"".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "abc".to_string() },
            0, 5,
        ),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![LexerLog::LineBreakInStrLiteral { span: Span::new(0, 5) }]);
}

#[test]
fn tokenizes_raw_str_literal() {
    let input = &mut r#"r"\\n""#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: r"\\n".to_string() },
            0, 6,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_byte_str_literal() {
    let input = &mut r#"b"abc""#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::ByteStr { value: "abc".to_string() },
            0, 6,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn tokenizes_raw_byte_str_literal() {
    let input = &mut r#"br"abc\""#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::ByteStr { value: r"abc\".to_string() },
            0, 8,
        ),
    ]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn detects_line_break_in_str_literal_and_skips_to_next_double_quot() {
    let input = &mut "\"abc\nskipped\nskipped\"tokenized".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "abc".to_string() },
            0, 4,
        ),
        id_token!("tokenized", 21, 9),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![LexerLog::LineBreakInStrLiteral { span: Span::new(0, 4) }]);
}

#[test]
fn detects_line_break_in_str_literal_and_skips_to_eof() {
    let input = &mut "\"abc\nskipped\nskipped".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "abc".to_string() },
            0, 4,
        ),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![LexerLog::LineBreakInStrLiteral { span: Span::new(0, 4) }]);
}

#[test]
fn detects_unclosed_str_literal() {
    let input = &mut r#""abc"#.into();
    let (tokens, logs) = Lexer::new().tokenize_(input);
    assert_eq!(tokens, vec![
        literal_token!(
            Literal::Str { value: "abc".to_string() },
            0, 4,
        ),
    ]);
    assert!(input.peek().is_none());
    assert_eq!(logs, vec![LexerLog::UnclosedStrLiteral { span: Span::new(0, 4) }]);
}

#[test]
fn consumes_unknown_char() {
    let input = &mut "\0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(tokens, vec![token!(Unknown, 0, 1)]);
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn ignores_continuous_unknown_chars() {
    let input = &mut "\0\0\0\n\0\0\0;\0\0\0".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(
        tokens,
        vec![
            token!(Unknown, 0, 3),
            token!(Unknown, 4, 3),
            token!(Semicolon, 7, 1),
            token!(Unknown, 8, 3),
        ],
    );
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}

#[test]
fn parses_unknown_token_with_multibyte_chars() {
    let input = &mut "カリン".into();
    let (tokens, logs) = Lexer::new().tokenize_(input);

    assert_eq!(
        tokens,
        vec![
            token!(Unknown, 0, 3),
        ],
    );
    assert!(input.peek().is_none());
    assert!(logs.is_empty());
}
