#[macro_export]
macro_rules! token {
    ($kind:ident, $index:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Token, TokenKind};
            Token::new(TokenKind::$kind, $index, $len)
        }
    };
}

#[macro_export]
macro_rules! id_token {
    ($id:expr, $index:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Token, TokenKind};
            Token::new(TokenKind::Id($id.to_string()), $index, $len)
        }
    };
}

#[macro_export]
macro_rules! keyword_token {
    ($keyword:ident, $index:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Keyword, Token, TokenKind};
            Token::new(TokenKind::Keyword(Keyword::$keyword), $index, $len)
        }
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub index: usize,
    pub len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, index: usize, len: usize) -> Token {
        Token { kind, index, len }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // basic tokens
    Id(String),
    Keyword(Keyword),
    Literal(Literal),

    // symbol tokens
    ClosingParen,
    ClosingCurlyBracket,
    Colon,
    Comma,
    DoubleColon,
    Equal,
    OpenCurlyBracket,
    OpenParen,
    Semicolon,

    // special tokens
    Unknown,
}


#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int { value: String },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Fn,
    Let,
    Pub,
    Struct,
    Usize,
}

impl Keyword {
    pub fn from(s: &str) -> Option<Keyword> {
        let keyword = match s {
            "fn" => Keyword::Fn,
            "let" => Keyword::Let,
            "pub" => Keyword::Pub,
            "struct" => Keyword::Struct,
            "usize" => Keyword::Usize,
            _ => return None,
        };

        Some(keyword)
    }
}
