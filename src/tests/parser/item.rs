use crate::{*, ir::hir::*, lexer::token::*, parser::{Parser, ParserCombinatoryResult, ParserLog}};

#[test]
fn parses_function() {
    let tokens = vec![
        (TokenPosition::default(), keyword!(Function)),
        (TokenPosition::default(), id!("f")),
        (TokenPosition::default(), symbol!(OpenParen)),
        (TokenPosition::default(), symbol!(ClosingParen)),
        (TokenPosition::default(), symbol!(OpenCurlyBracket)),
        (TokenPosition::default(), symbol!(ClosingCurlyBracket)),
    ];

    assert_eq!(
        Parser::new(&tokens).parse_item(),
        ParserCombinatoryResult::Matched(
            HirItem::Function(
                HirFunction {
                    id: "f".to_string(),
                },
            ),
        ),
    );
}

#[test]
fn skips_expected_function_identifier() {
    let tokens = vec![
        (TokenPosition::new(0, 1), keyword!(Function)),
        (TokenPosition::default(), symbol!(OpenParen)),
        (TokenPosition::default(), symbol!(ClosingParen)),
        (TokenPosition::default(), symbol!(OpenCurlyBracket)),
        (TokenPosition::default(), symbol!(ClosingCurlyBracket)),
    ];

    assert_eq!(
        Parser::new(&tokens).parse(),
        (
            Hir::new(),
            vec![ParserLog::ExpectedIdentifier(TokenPosition::new(1, 1))],
        ),
    );
}
