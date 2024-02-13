use crate::{*, ir::hir::*, lexer::token::*, parser::{Parser, ParserCombinatoryResult}};

#[test]
fn choices_any_expression() {
    let tokens = vec![
        (TokenPosition::default(), number!("0")),
    ];

    assert_eq!(
        Parser::new().parse_expression(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Matched(Some(HirExpression::Number(HirNumber("0".to_string())))),
    );

    let tokens = vec![
        (TokenPosition::default(), id!("id")),
    ];

    assert_eq!(
        Parser::new().parse_expression(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Matched(Some(HirExpression::Identifier(HirIdentifier("id".to_string())))),
    );
}

#[test]
fn matches_single_actual_argument() {
    let tokens = vec![
        (TokenPosition::default(), number!("0")),
    ];

    assert_eq!(
        Parser::new().parse_actual_arguments(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Matched(
            vec![
                HirActualArgument {
                    expr: HirExpression::Number(HirNumber("0".to_string())),
                },
            ],
        ),
    );
}

#[test]
fn matches_number() {
    let tokens = vec![
        (TokenPosition::default(), number!("0")),
    ];

    assert_eq!(
        Parser::new().parse_number(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Matched(HirNumber("0".to_string())),
    );
}

#[test]
fn matches_identifier() {
    let tokens = vec![
        (TokenPosition::default(), id!("f")),
    ];

    assert_eq!(
        Parser::new().parse_identifier(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Matched(HirIdentifier("f".to_string())),
    );
}

#[test]
fn does_not_match_identifier() {
    let tokens = vec![
        (TokenPosition::default(), symbol!(OpenParen)),
    ];

    assert_eq!(
        Parser::new().parse_identifier(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Unmatched,
    );

    let tokens = Vec::new();

    assert_eq!(
        Parser::new().parse_identifier(&mut tokens.iter().peekable()),
        ParserCombinatoryResult::Unmatched,
    );
}
