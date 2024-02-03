#[derive(Clone, Debug, PartialEq)]
pub struct TokenPosition {
    pub index: usize,
    pub len: usize,
}

impl TokenPosition {
    pub fn new(index: usize, len: usize) -> TokenPosition {
        TokenPosition { index, len }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Keyword(KeywordToken),
    Symbol(SymbolToken),
}

#[derive(Clone, Debug, PartialEq)]
pub enum KeywordToken {
    Function,
    Public,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolToken {
    OpenParen,
    ClosingParen,
    OpenCurlyBracket,
    ClosingCurlyBracket,
}
