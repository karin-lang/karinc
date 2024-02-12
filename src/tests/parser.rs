#[cfg(test)]
mod item;

use crate::{ir::hir::*, lexer::token::*, parser::{Parser, ParserCombinatoryResult}};

#[macro_export]
macro_rules! number {
    ($number:expr) => {
        Token::Number($number.to_string())
    };
}

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
                            id: HirIdentifier("f".to_string()),
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

#[test]
fn choices_any_expression() {
    let tokens = vec![
        (TokenPosition::default(), number!("0")),
    ];

    assert_eq!(
        Parser::new(&tokens).parse_expression(),
        ParserCombinatoryResult::Matched(Some(HirExpression::Number(HirNumber("0".to_string())))),
    );

    let tokens = vec![
        (TokenPosition::default(), id!("id")),
    ];

    assert_eq!(
        Parser::new(&tokens).parse_expression(),
        ParserCombinatoryResult::Matched(Some(HirExpression::Identifier(HirIdentifier("id".to_string())))),
    );
}

#[test]
fn matches_number() {
    let tokens = vec![
        (TokenPosition::default(), number!("0")),
    ];

    assert_eq!(
        Parser::new(&tokens).parse_number(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Matched(HirNumber("0".to_string())),
    );
}

#[test]
fn matches_identifier() {
    let tokens = vec![
        (TokenPosition::default(), id!("f")),
    ];

    assert_eq!(
        Parser::new(&tokens).parse_identifier(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Matched(HirIdentifier("f".to_string())),
    );
}

#[test]
fn does_not_match_identifier() {
    let tokens = vec![
        (TokenPosition::default(), symbol!(OpenParen)),
    ];

    assert_eq!(
        Parser::new(&tokens).parse_identifier(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Unmatched,
    );

    let tokens = Vec::new();

    assert_eq!(
        Parser::new(&tokens).parse_identifier(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Unmatched,
    );
}
