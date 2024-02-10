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
            Some(
                HirItem::Function(
                    HirFunction {
                        id: HirIdentifier("f".to_string()),
                    },
                ),
            ),
        ),
    );
}

#[test]
fn skips_expected_function_identifier() {
    let tokens = vec![
        (TokenPosition::default(), keyword!(Function)),
        (TokenPosition::new(0, 0, 0, 1), symbol!(OpenParen)),
        (TokenPosition::default(), symbol!(ClosingParen)),
        (TokenPosition::default(), symbol!(OpenCurlyBracket)),
        (TokenPosition::default(), symbol!(ClosingCurlyBracket)),
    ];

    assert_eq!(
        Parser::new(&tokens).parse(),
        (
            Hir::new(),
            vec![ParserLog::ExpectedIdentifier(TokenPosition::default())],
        ),
    );
}
