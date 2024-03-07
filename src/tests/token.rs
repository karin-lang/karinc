use crate::lexer::token::Keyword;

#[test]
fn converts_alphanumerics_keyword_to_keyword_token() {
    assert_eq!(Keyword::from("pub"), Some(Keyword::Pub));
}

#[test]
fn converts_non_alphanumerics_keyword_to_none() {
    assert_eq!(Keyword::from(";"), None);
}
