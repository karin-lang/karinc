pub mod token;

use std::str::Chars;
use token::*;

pub type LexerResult = (Vec<Token>, Vec<LexerLog>);

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {
    UnknownToken(TokenPosition, String),
}

#[derive(Clone, Debug)]
pub struct LexerInput<'a> {
    input: Chars<'a>,
    index: usize,
    line: usize,
    column: usize,
}

impl<'a> LexerInput<'a> {
    pub fn new(input: &str, index: usize, line: usize, column: usize) -> LexerInput {
        LexerInput {
            input: input.chars(),
            index,
            line,
            column,
        }
    }

    pub fn next(&mut self) -> Option<(TokenPosition, char)> {
        if let Some(char) = self.input.next() {
            let pos = TokenPosition::new(self.index, self.line, self.column, 1);

            self.index += 1;

            if char == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }

            Some((pos, char))
        } else {
            None
        }
    }
}

impl<'a> From<&'a str> for LexerInput<'a> {
    fn from(input: &'a str) -> Self {
        LexerInput {
            input: input.chars(),
            index: 0,
            line: 0,
            column: 0,
        }
    }
}

pub struct Lexer;

impl Lexer {
    pub fn tokenize(input: &str) -> LexerResult {
        let mut input = LexerInput::from(input);
        let mut logs = Vec::new();
        let mut tokens = Vec::new();

        while let Some((pos, char)) = input.next() {
            let new_token = match char {
                ' ' | '\t' | '\n' => continue,
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut id = char.to_string();

                    loop {
                        let latest_input = input.clone();

                        let id_char = match input.next() {
                            Some((_, v)) => v,
                            None => break,
                        };

                        match id_char {
                            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => id.push(id_char),
                            _ => {
                                input = latest_input;
                                break;
                            },
                        }
                    }

                    Lexer::to_keyword_or_identifier(id)
                },
                '(' => Token::Symbol(SymbolToken::OpenParen),
                ')' => Token::Symbol(SymbolToken::ClosingParen),
                '{' => Token::Symbol(SymbolToken::OpenCurlyBracket),
                '}' => Token::Symbol(SymbolToken::ClosingCurlyBracket),
                _ => {
                    logs.push(LexerLog::UnknownToken(pos, char.to_string()));
                    continue;
                },
            };

            tokens.push(new_token);
        }

        (tokens, logs)
    }

    fn to_keyword_or_identifier(id: String) -> Token {
        let keyword = match id.as_str() {
            "fn" => KeywordToken::Function,
            "pub" => KeywordToken::Public,
            _ => return Token::Identifier(id),
        };

        Token::Keyword(keyword)
    }
}
