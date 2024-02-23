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
    Id(String),
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

        TokenKind::Id(s.to_string())
    }

    pub fn expect_number(&self) -> &NumberToken {
        match self {
            TokenKind::Number(value) => value,
            _ => panic!("expected id token"),
        }
    }

    pub fn expect_id(&self) -> &String {
        match self {
            TokenKind::Id(id) => id,
            _ => panic!("expected id token"),
        }
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
    Let,
    Public,
    Struct,
}

impl KeywordToken {
    pub fn from(s: &str) -> Option<KeywordToken> {
        let keyword = match s {
            "fn" => KeywordToken::Function,
            "let" => KeywordToken::Let,
            "pub" => KeywordToken::Public,
            "struct" => KeywordToken::Struct,
            _ => return None,
        };

        Some(keyword)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolToken {
    Comma,
    Colon,
    DoubleColon,
    Equal,
    Semicolon,
    OpenParen,
    ClosingParen,
    OpenCurlyBracket,
    ClosingCurlyBracket,
}
