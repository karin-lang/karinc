use std::fmt;

#[macro_export]
macro_rules! token {
    ($kind:ident, $line:expr, $column:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Token, TokenKind};
            Token::new(TokenKind::$kind, Span::new($line, $column, $len))
        }
    };
}

#[macro_export]
macro_rules! id_token {
    ($id:expr, $line:expr, $column:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Token, TokenKind};
            Token::new(TokenKind::Id($id.to_string()), Span::new($line, $column, $len))
        }
    };
}

#[macro_export]
macro_rules! keyword_token {
    ($keyword:ident, $line:expr, $column:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Keyword, Token, TokenKind};
            Token::new(TokenKind::Keyword(Keyword::$keyword), Span::new($line, $column, $len))
        }
    };
}

#[macro_export]
macro_rules! literal_token {
    ($literal:expr, $line:expr, $column:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Token, TokenKind};
            Token::new(TokenKind::Literal($literal), Span::new($line, $column, $len))
        }
    };
}

#[derive(Clone, PartialEq)]
pub struct Span {
    pub line: u16,
    pub column: u16,
    pub len: u16,
}

impl Span {
    pub fn new(line: u16, column: u16, len: u16) -> Span {
        Span { line, column, len }
    }

    pub fn from_usize(line: usize, column: usize, len: usize) -> Span {
        Span { line: line as u16, column: column as u16, len: len as u16 }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}({})", self.line, self.column, self.len)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::new(0, 0, 0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
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
    Mut,
    Pub,
    Struct,
    Usize,
}

impl Keyword {
    pub fn from(s: &str) -> Option<Keyword> {
        let keyword = match s {
            "fn" => Keyword::Fn,
            "let" => Keyword::Let,
            "mut" => Keyword::Mut,
            "pub" => Keyword::Pub,
            "struct" => Keyword::Struct,
            "usize" => Keyword::Usize,
            _ => return None,
        };

        Some(keyword)
    }
}
