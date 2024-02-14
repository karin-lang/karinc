use crate::new::data::token::{KeywordToken, NumberToken, TokenKind};

#[test]
fn converts_alphanumerics_to_number_token() {
    assert_eq!(
        TokenKind::from_alphanumerics("0"),
        TokenKind::Number(NumberToken("0".to_string())),
    );
}

#[test]
fn converts_alphanumerics_to_id_token() {
    assert_eq!(
        TokenKind::from_alphanumerics("id"),
        TokenKind::Identifier("id".to_string()),
    );
}

#[test]
fn converts_non_alphanumerics_to_id_token_instead() {
    assert_eq!(
        TokenKind::from_alphanumerics(";"),
        TokenKind::Identifier(";".to_string()),
    );
}

#[test]
fn converts_alphanumerics_to_keyword_token() {
    assert_eq!(
        TokenKind::from_alphanumerics("pub"),
        TokenKind::Keyword(KeywordToken::Public),
    );
}

#[test]
fn converts_numerics_to_number_token() {
    assert_eq!(NumberToken::from("0"), Some(NumberToken("0".to_string())));
}

#[test]
fn converts_non_numerics_to_none() {
    assert_eq!(NumberToken::from(";"), None);
}

#[test]
fn converts_alphanumerics_keyword_to_keyword_token() {
    assert_eq!(KeywordToken::from("pub"), Some(KeywordToken::Public));
}

#[test]
fn converts_non_alphanumerics_keyword_to_none() {
    assert_eq!(KeywordToken::from(";"), None);
}
