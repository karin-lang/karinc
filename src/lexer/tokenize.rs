use std::iter::{Enumerate, Peekable};
use std::str::Chars;

use crate::{lexer::token::*, parser::ast};
use crate::lexer::log::{LexerLog, LexerResult};

pub struct LexerInput<'a> {
    input: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> LexerInput<'a> {
    pub fn next(&mut self) -> Option<(usize, char)> {
        self.input.next()
    }

    pub fn peek(&mut self) -> Option<&(usize, char)> {
        self.input.peek()
    }
}

impl<'a> From<&'a str> for LexerInput<'a> {
    fn from(value: &'a str) -> Self {
        Self { input: value.chars().enumerate().peekable() }
    }
}

pub struct Lexer {
    pub(crate) logs: Vec<LexerLog>,
    pub(crate) newline_indexes: Vec<u16>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            logs: Vec::new(),
            newline_indexes: Vec::new(),
        }
    }

    pub fn record_log(&mut self, log: LexerLog) {
        self.logs.push(log);
    }

    pub fn record_result_log<T>(&mut self, result: LexerResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(log) => {
                self.record_log(log);
                None
            }
        }
    }

    pub fn add_newline_index(&mut self, index: usize) {
        self.newline_indexes.push(index as u16);
    }

    pub fn tokenize(self, input: &str) -> (Vec<Token>, Vec<LexerLog>) {
        let input = &mut input.into();
        self.tokenize_(input)
    }

    pub(crate) fn tokenize_(mut self, input: &mut LexerInput) -> (Vec<Token>, Vec<LexerLog>) {
        let mut tokens = Vec::new();
        let mut last_unknown_token_span: Option<Span> = None;

        loop {
            let (index, next_char) = match input.next() {
                Some(v) => v,
                None => break,
            };
            // note: 連続する不明な文字を 1 トークンに統合するためのフラグ
            let mut is_unknown_token = false;

            let token: Option<(usize, TokenKind)> = match next_char {
                ' ' | '\t' => None,
                '\n' => {
                    self.add_newline_index(index);
                    None
                },
                '\'' => Some(self.tokenize_char_literal(input, index, 1)),
                '"' => Some(self.tokenize_str_literal(input, index, 1)),
                'b' => match input.peek() {
                    Some((_, '\'')) => {
                        input.next();
                        Some(self.tokenize_byte_char_literal(input, index, 2))
                    },
                    Some((_, '"')) => {
                        input.next();
                        Some(self.tokenize_byte_str_literal(input, index, 2))
                    },
                    Some((_, 'r')) => {
                        input.next();
                        if let Some((_, '"')) = input.peek() {
                            input.next();
                            Some(self.tokenize_raw_byte_str_literal(input, index, 3))
                        } else {
                            Some(Lexer::tokenize_alphabetics(input, Some("br".to_string())))
                        }
                    },
                    _ => Some(Lexer::tokenize_alphabetics(input, Some("b".to_string()))),
                },
                'r' => if let Some((_, '"')) = input.peek() {
                    input.next();
                    Some(self.tokenize_raw_str_literal(input, index, 2))
                } else {
                    Some(Lexer::tokenize_alphabetics(input, Some(next_char.to_string())))
                },
                'a'..='z' | 'A'..='Z' | '_' => Some(Lexer::tokenize_alphabetics(input, Some(next_char.to_string()))),
                '0'..='9' => {
                    let mut int_digits = String::new();
                    let mut fraction_digits = String::new();
                    let mut is_float = false;
                    let mut last_index = index;

                    // note: 0 で始まる数値リテラルから接頭辞を発見する
                    // note: 接頭辞が見つかった場合は整数開始位置に進み、そうでなかった場合は最初の1桁を消費した状態まで進む
                    let base = if next_char == '0' {
                        match input.peek() {
                            Some((index, 'b')) => { last_index = *index; input.next(); Base::Bin },
                            Some((index, 'o')) => { last_index = *index; input.next(); Base::Oct },
                            Some((index, 'x')) => { last_index = *index; input.next(); Base::Hex },
                            _ => { int_digits.push(next_char); Base::Dec },
                        }
                    } else {
                        int_digits.push(next_char);
                        Base::Dec
                    };

                    loop {
                        match input.peek() {
                            Some((index, next_char @ ('0'..='9' | 'a'..='f' | 'A'..='F' | '_'))) => {
                                if is_float {
                                    fraction_digits.push(*next_char);
                                } else {
                                    int_digits.push(*next_char);
                                }
                                last_index = *index;
                            },
                            Some((index, '.')) => {
                                if is_float {
                                    break;
                                }
                                is_float = true;
                                last_index = *index;
                            },
                            _ => break,
                        }
                        input.next();
                    }

                    let (r#type, expected_type_suffix) = match input.peek() {
                        Some((_, 'a'..='z' | 'A'..='Z')) => {
                            let alphabetics = Lexer::consume_alphabetics(input, None);
                            last_index += alphabetics.len();
                            match ast::PrimType::from(&alphabetics) {
                                Some(prim_type) => (Some(prim_type), false),
                                None => (None, true),
                            }
                        },
                        _ => (None, false),
                    };

                    let len = last_index - index + 1;

                    if expected_type_suffix {
                        self.record_log(LexerLog::ExpectedTypeSuffix { span: Span::from_usize(index, len) });
                    }

                    let literal = if is_float {
                        let digits = if base == Base::Dec {
                            Some(FloatDigits { int: int_digits, fraction: fraction_digits })
                        } else {
                            self.record_log(LexerLog::ExpectedDecimalFloat { span: Span::from_usize(index, len) });
                            None
                        };
                        Literal::Float { digits, r#type }
                    } else {
                        Literal::Int { base, int_digits, r#type }
                    };
                    Some((len, TokenKind::Literal(literal)))
                },
                '}' => Some((1, TokenKind::ClosingCurlyBracket)),
                ')' => Some((1, TokenKind::ClosingParen)),
                ':' => if let Some((_, ':')) = input.peek() {
                    input.next();
                    Some((2, TokenKind::DoubleColon))
                } else {
                    Some((1, TokenKind::Colon))
                },
                ',' => Some((1, TokenKind::Comma)),
                '.' => Some((1, TokenKind::Dot)),
                '=' => Some((1, TokenKind::Equal)),
                '{' => Some((1, TokenKind::OpenCurlyBracket)),
                '(' => Some((1, TokenKind::OpenParen)),
                ';' => Some((1, TokenKind::Semicolon)),
                _ => {
                    match &mut last_unknown_token_span {
                        Some(span) => span.len += 1,
                        None => last_unknown_token_span = Some(Span::from_usize(index, 1)),
                    }
                    // note: EOF の直前に位置する場合は字句解析を終了する前に Unknown トークンを追加する
                    if input.peek().is_none() {
                        if let Some(span) = &last_unknown_token_span {
                            tokens.push(Token::new(TokenKind::Unknown, span.clone()));
                            last_unknown_token_span = None;
                        }
                    }
                    is_unknown_token = true;
                    None
                },
            };

            if !is_unknown_token {
                if let Some(span) = &last_unknown_token_span {
                    tokens.push(Token::new(TokenKind::Unknown, span.clone()));
                    last_unknown_token_span = None;
                }
            }
            if let Some((len, kind)) = token {
                let span = Span::from_usize(index, len);
                tokens.push(Token::new(kind, span));
            }
        }

        (tokens, self.logs)
    }

    fn consume_alphabetics(input: &mut LexerInput, initial: Option<String>) -> String {
        let mut alphabetics = initial.unwrap_or_default();
        while let Some((_, next_char @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_'))) = input.peek() {
            alphabetics.push(*next_char);
            input.next();
        }
        alphabetics
    }

    fn tokenize_alphabetics(input: &mut LexerInput, initial: Option<String>) -> (usize, TokenKind) {
        let alphabetics = Lexer::consume_alphabetics(input, initial);
        let len = alphabetics.len();

        if alphabetics == "void" {
            return (len, TokenKind::Literal(Literal::Void));
        }

        if let Some(value) = Literal::to_bool_literal(&alphabetics) {
            return (len, TokenKind::Literal(Literal::Bool { value }));
        }

        if let Some(keyword) = Keyword::from(&alphabetics) {
            return (len, TokenKind::Keyword(keyword));
        }

        if let Some(prim_type) = ast::PrimType::from(&alphabetics) {
            return (len, TokenKind::PrimType(prim_type));
        }

        (len, TokenKind::Id(alphabetics))
    }

    fn tokenize_char_or_str_literal_(
        &mut self,
        input: &mut LexerInput,
        begin: usize,
        backward_len: usize,
        is_char: bool,
        is_raw: bool,
        is_byte: bool,
    ) -> (usize, TokenKind) {
        let mut value = String::new();
        let mut len = backward_len;

        loop {
            match input.next() {
                Some((_, '\'')) => {
                    len += 1;
                    if is_char {
                        break;
                    }
                    value.push('\'');
                },
                Some((_, '"')) => {
                    len += 1;
                    if !is_char {
                        break;
                    }
                    value.push('"');
                },
                Some((escseq_index, '\\')) => {
                    len += 1;
                    if is_raw {
                        value.push('\\');
                    } else {
                        // todo: Unicode エスケープシーケンスを追加する
                        match input.peek() {
                            Some((_, '\\')) => { input.next(); len += 1; value.push('\\') },
                            Some((_, '\'')) => { input.next(); len += 1; value.push('\'') },
                            Some((_, '"')) => { input.next(); len += 1; value.push('"') },
                            Some((_, '0')) => { input.next(); len += 1; value.push('\0') },
                            Some((_, 'r')) => { input.next(); len += 1; value.push('\r') },
                            Some((_, 't')) => { input.next(); len += 1; value.push('\t') },
                            Some((_, 'n')) => { input.next(); len += 1; value.push('\n') },
                            // note: エスケープ文字に改行が出現した場合は入力位置を進めずに LineBreakInCharLiteral / LineBreakInStrLiteral ログの出力を委ねる
                            Some((_, '\n')) => continue,
                            Some((_, _)) => {
                                input.next();
                                len += 1;
                                let span = Span::from_usize(escseq_index, 2);
                                self.record_log(LexerLog::UnknownEscseq { span });
                            },
                            // note: EOF の場合は UnclosedCharLiteral / UnclosedStrLiteral ログの出力を委ねる
                            None => continue,
                        }
                    }
                },
                // note: リテラル中に改行を検知した場合は次のクォーテーションまで入力位置を進める
                Some((index, '\n')) => {
                    self.add_newline_index(index);

                    let span = Span::from_usize(begin, len);
                    let log = if is_char {
                        LexerLog::LineBreakInCharLiteral { span }
                    } else {
                        LexerLog::LineBreakInStrLiteral { span }
                    };
                    self.record_log(log);

                    loop {
                        match input.next() {
                            Some((_, '"')) if !is_char => break,
                            Some((_, '\'')) if is_char => break,
                            Some((index, '\n')) => self.add_newline_index(index),
                            None => break,
                            _ => (),
                        }
                    }
                    // note: 不要な UnclosedCharLiteral / UnclosedStrLiteral ログを回避する
                    break;
                },
                Some((_, next_char)) => {
                    len += 1;
                    value.push(next_char);
                },
                None => {
                    let span = Span::from_usize(begin, len);
                    let log = if is_char {
                        LexerLog::UnclosedCharLiteral { span }
                    } else {
                        LexerLog::UnclosedStrLiteral { span }
                    };
                    self.record_log(log);
                    break;
                },
            }
        }

        let literal = if is_char {
            let span = Span::from_usize(begin, len);
            let value = match value.chars().next() {
                Some(ch) if value.chars().count() == 1 => Some(ch),
                Some(_) => {
                    self.record_log(LexerLog::TooLongCharLiteral { span });
                    None
                },
                None => {
                    self.record_log(LexerLog::EmptyCharLiteral { span });
                    None
                },
            };
            if is_byte {
                Literal::ByteChar { value }
            } else {
                Literal::Char { value }
            }
        } else {
            if is_byte {
                Literal::ByteStr { value }
            } else {
                Literal::Str { value }
            }
        };
        (len, TokenKind::Literal(literal))
    }

    fn tokenize_char_literal(&mut self, input: &mut LexerInput, begin: usize, backward_len: usize) -> (usize, TokenKind) {
        self.tokenize_char_or_str_literal_(input, begin, backward_len, true, false, false)
    }

    fn tokenize_byte_char_literal(&mut self, input: &mut LexerInput, begin: usize, backward_len: usize) -> (usize, TokenKind) {
        self.tokenize_char_or_str_literal_(input, begin, backward_len, true, false, true)
    }

    fn tokenize_str_literal(&mut self, input: &mut LexerInput, begin: usize, backward_len: usize) -> (usize, TokenKind) {
        self.tokenize_char_or_str_literal_(input, begin, backward_len, false, false, false)
    }

    fn tokenize_raw_str_literal(&mut self, input: &mut LexerInput, begin: usize, backward_len: usize) -> (usize, TokenKind) {
        self.tokenize_char_or_str_literal_(input, begin, backward_len, false, true, false)
    }

    fn tokenize_byte_str_literal(&mut self, input: &mut LexerInput, begin: usize, backward_len: usize) -> (usize, TokenKind) {
        self.tokenize_char_or_str_literal_(input, begin, backward_len, false, false, true)
    }

    fn tokenize_raw_byte_str_literal(&mut self, input: &mut LexerInput, begin: usize, backward_len: usize) -> (usize, TokenKind) {
        self.tokenize_char_or_str_literal_(input, begin, backward_len, false, true, true)
    }
}
