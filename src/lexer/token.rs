#[macro_export]
macro_rules! id_token {
    ($id:expr, $index:expr, $len:expr$(,)?) => {
        Token::new(TokenKind::Id($id.to_string()), $index, $len)
    };
}

#[macro_export]
macro_rules! symbol_token {
    ($symbol:ident, $index:expr, $len:expr$(,)?) => {
        Token::new(TokenKind::Symbol(SymbolToken::$symbol), $index, $len)
    };
}

#[macro_export]
macro_rules! keyword_token {
    ($keyword:ident, $index:expr, $len:expr$(,)?) => {
        Token::new(TokenKind::Keyword(KeywordToken::$keyword), $index, $len)
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

// todo: KeywordToken → TokenKeyword
// todo: バリアント名を変える
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

// todo: SymbolToken → TokenSymbol
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
