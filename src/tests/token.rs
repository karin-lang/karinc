use crate::lexer::{token::TokenPosition, LexerInput};

#[test]
fn adds_index_and_column_when_consumes_char() {
    let mut input = LexerInput::from("ab");
    let char_indice = input.next();
    assert_eq!(char_indice, Some((TokenPosition::new(0, 0, 0, 1), 'a')));
    let char_indice = input.next();
    assert_eq!(char_indice, Some((TokenPosition::new(1, 0, 1, 1), 'b')));
}

#[test]
fn adds_line_and_resets_column_when_consumes_newline_char() {
    let mut input = LexerInput::from("\na");
    let char_indice = input.next();
    assert_eq!(char_indice, Some((TokenPosition::new(0, 0, 0, 1), '\n')));
    let char_indice = input.next();
    assert_eq!(char_indice, Some((TokenPosition::new(1, 1, 0, 1), 'a')));
}
