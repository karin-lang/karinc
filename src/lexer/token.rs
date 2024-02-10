#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TokenPosition {
    pub index: usize,
    pub line: usize,
    pub column: usize,
    pub len: usize,
}

impl TokenPosition {
    pub fn new(index: usize, line: usize, column: usize, len: usize) -> TokenPosition {
        TokenPosition { index, line, column, len }
    }

    pub fn add(&self, index: usize, len: usize) -> TokenPosition {
        TokenPosition {
            index: self.index + index,
            line: self.line,
            column: self.column + index,
            len,
        }
    }
}

impl Default for TokenPosition {
    fn default() -> Self {
        TokenPosition { index: 0, line: 0, column: 0, len: 0 }
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
    Semicolon,
    OpenParen,
    ClosingParen,
    OpenCurlyBracket,
    ClosingCurlyBracket,
}
