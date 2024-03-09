use std::{iter::Peekable, str::CharIndices};
use crate::lexer::token::*;

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {}

pub type LexerResult<T> = Result<T, LexerLog>;

pub struct Lexer {
    pub(crate) logs: Vec<LexerLog>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            logs: Vec::new(),
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

    pub fn tokenize(self, input: &str) -> (Vec<Token>, Vec<LexerLog>) {
        let input = &mut input.char_indices().peekable();
        self.tokenize_(input)
    }

    pub(crate) fn tokenize_(self, input: &mut Peekable<CharIndices>) -> (Vec<Token>, Vec<LexerLog>) {
        let mut tokens = Vec::new();
        let mut line: usize = 0;
        let mut start_index_of_line: usize = 0;

        loop {
            let (index, next_char) = match input.next() {
                Some(v) => v,
                None => break,
            };
            let column = index - start_index_of_line;

            let (len, kind): (usize, TokenKind) = match next_char {
                ' ' | '\t' => continue,
                '\n' => {
                    line += 1;
                    start_index_of_line = index + 1;
                    continue;
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut alphabetic = next_char.to_string();

                    loop {
                        match input.peek() {
                            Some((_, next_char @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_'))) => {
                                alphabetic.push(*next_char);
                                input.next();
                            },
                            _ => break,
                        };
                    }

                    let len = alphabetic.len();
                    let kind = match Keyword::from(&alphabetic) {
                        Some(keyword) => TokenKind::Keyword(keyword),
                        None => TokenKind::Id(alphabetic),
                    };
                    (len, kind)
                },
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
                            Some((index, next_char @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_'))) => {
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

                    let len = last_index - index + 1;
                    let literal = if is_float {
                        Literal::Float { base, int_digits, fraction_digits }
                    } else {
                        Literal::Int { base, int_digits }
                    };
                    (len, TokenKind::Literal(literal))
                },
                ')' => (1, TokenKind::ClosingParen),
                '}' => (1, TokenKind::ClosingCurlyBracket),
                ':' => {
                    let colon = (1, TokenKind::Colon);
                    if let Some((_, second_char)) = input.peek().cloned() {
                        if second_char == ':' {
                            input.next();
                            (2, TokenKind::DoubleColon)
                        } else {
                            colon
                        }
                    } else {
                        colon
                    }
                },
                ',' => (1, TokenKind::Comma),
                '.' => (1, TokenKind::Dot),
                '=' => (1, TokenKind::Equal),
                '{' => (1, TokenKind::OpenCurlyBracket),
                '(' => (1, TokenKind::OpenParen),
                ';' => (1, TokenKind::Semicolon),
                _ => {
                    // todo: 連続した不明なトークンをまとめて扱う＆テストを追加する
                    (1, TokenKind::Unknown)
                },
            };

            let span = Span::from_usize(line, column, len);
            tokens.push(Token::new(kind, span));
        }

        (tokens, self.logs)
    }
}
