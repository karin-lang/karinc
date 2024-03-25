use std::fmt;
use crate::parser::ast;

#[macro_export]
macro_rules! token {
    ($kind:ident, $begin:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Token, TokenKind};
            Token::new(TokenKind::$kind, Span::new($begin, $len))
        }
    };
}

#[macro_export]
macro_rules! id_token {
    ($id:expr, $begin:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Token, TokenKind};
            Token::new(TokenKind::Id($id.to_string()), Span::new($begin, $len))
        }
    };
}

#[macro_export]
macro_rules! keyword_token {
    ($keyword:ident, $begin:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Keyword, Token, TokenKind};
            Token::new(TokenKind::Keyword(Keyword::$keyword), Span::new($begin, $len))
        }
    };
}

#[macro_export]
macro_rules! prim_type_token {
    ($prim_type:ident, $begin:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Token, TokenKind};
            use crate::parser::ast::PrimType;
            Token::new(TokenKind::PrimType(PrimType::$prim_type), Span::new($begin, $len))
        }
    };
}

#[macro_export]
macro_rules! literal_token {
    ($literal:expr, $begin:expr, $len:expr$(,)?) => {
        {
            use crate::lexer::token::{Span, Token, TokenKind};
            Token::new(TokenKind::Literal($literal), Span::new($begin, $len))
        }
    };
}

#[derive(Clone, PartialEq)]
pub struct Span {
    pub begin: u16,
    pub len: u16,
}

impl Span {
    pub fn new(begin: u16, len: u16) -> Span {
        Span { begin, len }
    }

    pub fn from_usize(begin: usize, len: usize) -> Span {
        Span { begin: begin as u16, len: len as u16 }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.begin, self.begin + self.len)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::new(0, 0)
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
    PrimType(ast::PrimType),
    Literal(Literal),

    // symbol tokens
    ClosingCurlyBracket,
    ClosingParen,
    Colon,
    Comma,
    Dot,
    DoubleColon,
    Equal,
    OpenCurlyBracket,
    OpenParen,
    Semicolon,

    // special tokens
    Unknown,
}

impl ast::PrimType {
    pub fn from(s: &str) -> Option<ast::PrimType> {
        let prim_type = match s {
            "bool" => ast::PrimType::Bool,
            "i8" => ast::PrimType::I8,
            "i16" => ast::PrimType::I16,
            "i32" => ast::PrimType::I32,
            "i64" => ast::PrimType::I64,
            "isize" => ast::PrimType::Isize,
            "u8" => ast::PrimType::U8,
            "u16" => ast::PrimType::U16,
            "u32" => ast::PrimType::U32,
            "u64" => ast::PrimType::U64,
            "usize" => ast::PrimType::Usize,
            "f32" => ast::PrimType::F32,
            "f64" => ast::PrimType::F64,
            "char" => ast::PrimType::Char,
            "str" => ast::PrimType::Str,
            _ => return None,
        };
        Some(prim_type)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool { value: bool },
    Int { base: Base, int_digits: String, r#type: Option<ast::PrimType> },
    Float { base: Base, int_digits: String, fraction_digits: String, r#type: Option<ast::PrimType> },
    Char { value: Option<char> },
    Str { value: String },
    ByteChar { value: Option<char> },
    ByteStr { value: String },
}

impl Literal {
    pub fn to_bool_literal(s: &str) -> Option<bool> {
        match s {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Base {
    Bin,
    Oct,
    Dec,
    Hex,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Fn,
    Let,
    Mut,
    Pub,
    Struct,
}

impl Keyword {
    pub fn from(s: &str) -> Option<Keyword> {
        let keyword = match s {
            "fn" => Keyword::Fn,
            "let" => Keyword::Let,
            "mut" => Keyword::Mut,
            "pub" => Keyword::Pub,
            "struct" => Keyword::Struct,
            _ => return None,
        };
        Some(keyword)
    }
}
