use std::{iter::Peekable, str::CharIndices};
use crate::lexer::token::*;

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {}

pub struct Lexer {
    pub(crate) logs: Vec<LexerLog>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            logs: Vec::new(),
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
                    let mut numeric = next_char.to_string();

                    loop {
                        match input.peek() {
                            Some((_, next_char @ '0'..='9')) => {
                                numeric.push(*next_char);
                                input.next();
                            },
                            _ => break,
                        };
                    }

                    let len = numeric.len();
                    let literal = Literal::Int { value: numeric };
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
                '=' => (1, TokenKind::Equal),
                '{' => (1, TokenKind::OpenCurlyBracket),
                '(' => (1, TokenKind::OpenParen),
                ';' => (1, TokenKind::Semicolon),
                _ => {
                    // todo: 連続した不明なトークンをまとめて扱う
                    (1, TokenKind::Unknown)
                },
            };

            let span = Span::from_usize(line, column, len);
            tokens.push(Token::new(kind, span));
        }

        (tokens, self.logs)
    }
}
