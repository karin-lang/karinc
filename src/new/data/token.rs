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
    Number(NumberToken),
    Identifier(String),
    Keyword(KeywordToken),
    Symbol(SymbolToken),
}

impl TokenKind {
    pub fn from_alphanumerics(s: &str) -> TokenKind {
        if let Some(number) = NumberToken::from(s) {
            return TokenKind::Number(number);
        };

        if let Some(keyword) = KeywordToken::from(s) {
            return TokenKind::Keyword(keyword);
        };

        TokenKind::Identifier(s.to_string())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NumberToken(pub String);

impl NumberToken {
    pub fn from(s: &str) -> Option<NumberToken> {
        if let Some(first_char) = s.chars().next() {
            match first_char {
                '0'..='9' => Some(NumberToken(s.to_string())),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum KeywordToken {
    Function,
    Public,
}

impl KeywordToken {
    pub fn from(s: &str) -> Option<KeywordToken> {
        let keyword = match s {
            "fn" => KeywordToken::Function,
            "pub" => KeywordToken::Public,
            _ => return None,
        };

        Some(keyword)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolToken {
    Colon,
    DoubleColon,
    Semicolon,
    OpenParen,
    ClosingParen,
}
