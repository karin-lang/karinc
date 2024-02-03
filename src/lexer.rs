pub mod token;

use token::*;

pub type LexerResult = (Vec<Token>, Vec<LexerLog>);

#[derive(Clone, Debug, PartialEq)]
pub enum LexerLog {
    UnknownToken(TokenPosition, String),
}

pub struct Lexer;

impl Lexer {
    pub fn tokenize(input: &str) -> LexerResult {
        let mut input = input.char_indices();
        let mut logs = Vec::new();
        let mut tokens = Vec::new();

        while let Some((index, char)) = input.next() {
            let new_token = match char {
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
                    logs.push(LexerLog::UnknownToken(TokenPosition::new(index, 1), char.to_string()));
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
