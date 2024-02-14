use std::{iter::Peekable, str::CharIndices};
use crate::new::data::token::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {
    UnexpectedToken(usize, String),
}

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

    pub(crate) fn tokenize_(mut self, input: &mut Peekable<CharIndices>) -> (Vec<Token>, Vec<LexerLog>) {
        let mut tokens = Vec::new();

        loop {
            let (index, next_char) = match input.peek() {
                Some(v) => *v,
                None => break,
            };

            if let Some(alphanumerics) = self.tokenize_alphanumerics(input) {
                let new_token_kind = TokenKind::from_alphanumerics(&alphanumerics);
                let new_token = Token::new(new_token_kind, index, alphanumerics.len());
                tokens.push(new_token);
                continue;
            }

            self.logs.push(LexerLog::UnexpectedToken(index, next_char.to_string()));
            input.next();
        }

        (tokens, self.logs)
    }

    pub fn tokenize_alphanumerics(&mut self, input: &mut Peekable<CharIndices>) -> Option<String> {
        let mut alphanumerics = String::new();

        loop {
            let next_char = match input.peek() {
                Some((_, v)) => *v,
                None => break,
            };

            match next_char {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                    input.next();
                    alphanumerics.push(next_char);
                },
                _ => break,
            }
        }

        if alphanumerics.len() == 0 {
            None
        } else {
            Some(alphanumerics)
        }
    }
}
