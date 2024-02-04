#[cfg(test)]
mod item;

use crate::{ir::hir::*, lexer::token::*, parser::Parser};

#[macro_export]
macro_rules! id {
    ($id:expr) => {
        Token::Identifier($id.to_string())
    };
}

#[macro_export]
macro_rules! keyword {
    ($keyword:ident) => {
        Token::Keyword(KeywordToken::$keyword)
    };
}

#[macro_export]
macro_rules! symbol {
    ($symbol:ident) => {
        Token::Symbol(SymbolToken::$symbol)
    };
}

#[test]
fn generates_empty_hir() {
    assert_eq!(
        Parser::new(&Vec::new()).parse(),
        (Hir::new(), Vec::new()),
    );
}

#[test]
fn choices_single_item() {
    let tokens = vec![
        (TokenPosition::default(), keyword!(Function)),
        (TokenPosition::default(), id!("f")),
        (TokenPosition::default(), symbol!(OpenParen)),
        (TokenPosition::default(), symbol!(ClosingParen)),
        (TokenPosition::default(), symbol!(OpenCurlyBracket)),
        (TokenPosition::default(), symbol!(ClosingCurlyBracket)),
    ];

    assert_eq!(
        Parser::new(&tokens).parse(),
        (
            Hir {
                items: vec![
                    HirItem::Function(
                        HirFunction {
                            id: "f".to_string(),
                        },
                    ),
                ],
            },
            Vec::new(),
        ),
    );
}

#[test]
fn skips_unknown_syntax_token_with_error() {
    // let tokens = vec![
    //     (TokenPosition::default(), keyword!(Function)),
    // ];

    // assert_eq!(
    //     Parser::new(&tokens).parse(),
    //     (
    //         Hir {
    //             items: vec![
    //                 HirItem::Function(
    //                     HirFunction {
    //                         id: "f".to_string(),
    //                     },
    //                 ),
    //             ],
    //         },
    //         Vec::new(),
    //     ),
    // );
}
