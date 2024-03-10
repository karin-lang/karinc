use std::{iter::Peekable, str::CharIndices};
use crate::{lexer::token::*, parser::ast};

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {
    ExpectedTypeSuffix { span: Span },
}

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

    pub(crate) fn tokenize_(mut self, input: &mut Peekable<CharIndices>) -> (Vec<Token>, Vec<LexerLog>) {
        let mut tokens = Vec::new();
        let mut line: usize = 0;
        let mut start_index_of_line: usize = 0;
        let mut last_unknown_token_span: Option<Span> = None;

        loop {
            let (index, next_char) = match input.next() {
                Some(v) => v,
                None => break,
            };
            let column = index - start_index_of_line;
            // note: 連続する不明な文字を 1 トークンに統合するためのフラグ
            let mut is_unknown_token = false;

            let token: Option<(usize, TokenKind)> = match next_char {
                ' ' | '\t' => None,
                '\n' => {
                    line += 1;
                    start_index_of_line = index + 1;
                    None
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let alphabetic = Lexer::tokenize_id(input, Some(next_char));
                    let len = alphabetic.len();
                    let kind = match Keyword::from(&alphabetic) {
                        Some(keyword) => TokenKind::Keyword(keyword),
                        None => match ast::PrimType::from(&alphabetic) {
                            Some(prim_type) => TokenKind::PrimType(prim_type),
                            None => TokenKind::Id(alphabetic),
                        },
                    };
                    Some((len, kind))
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
                            let alphabetic = Lexer::tokenize_id(input, None);
                            last_index += alphabetic.len();
                            match ast::PrimType::from(&alphabetic) {
                                Some(prim_type) => (Some(prim_type), false),
                                None => (None, true),
                            }
                        },
                        _ => (None, false),
                    };

                    let len = last_index - index + 1;

                    if expected_type_suffix {
                        self.record_log(LexerLog::ExpectedTypeSuffix { span: Span::from_usize(line, column, len) });
                    }

                    let literal = if is_float {
                        Literal::Float { base, int_digits, fraction_digits, r#type }
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
                        None => last_unknown_token_span = Some(Span::from_usize(line, column, 1)),
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
                let span = Span::from_usize(line, column, len);
                tokens.push(Token::new(kind, span));
            }
        }

        (tokens, self.logs)
    }

    fn tokenize_id(input: &mut Peekable<CharIndices>, initial: Option<char>) -> String {
        let mut alphabetic = match initial {
            Some(ch) => ch.to_string(),
            None => String::new(),
        };
        loop {
            match input.peek() {
                Some((_, next_char @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_'))) => {
                    alphabetic.push(*next_char);
                    input.next();
                },
                _ => break,
            };
        }
        alphabetic
    }
}
