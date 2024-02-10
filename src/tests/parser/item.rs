use crate::{*, ir::hir::*, lexer::token::*, parser::{Parser, ParserCombinatoryResult, ParserLog}};

#[test]
fn parses_function_definition() {
    let tokens = vec![
        (TokenPosition::default(), keyword!(Function)),
        (TokenPosition::default(), id!("f")),
        (TokenPosition::default(), symbol!(OpenParen)),
        (TokenPosition::default(), symbol!(ClosingParen)),
        (TokenPosition::default(), symbol!(OpenCurlyBracket)),
        (TokenPosition::default(), symbol!(ClosingCurlyBracket)),
    ];

    assert_eq!(
        Parser::new(&tokens).parse_item_definition(),
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
fn ignores_function_definition_with_undefined_identifier() {
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
            vec![ParserLog::ExpectedIdentifier(TokenPosition::new(0, 0, 0, 1))],
        ),
    );
}

#[test]
fn parses_function_call() {
    let tokens = vec![
        (TokenPosition::default(), id!("f")),
        (TokenPosition::default(), symbol!(OpenParen)),
        (TokenPosition::default(), symbol!(ClosingParen)),
        (TokenPosition::default(), symbol!(Semicolon)),
    ];

    let mut parser = Parser::new(&tokens);
    let result = parser.parse_function_call();

    assert_eq!(
        result,
        ParserCombinatoryResult::Matched(
            HirFunctionCall {
                id: HirIdentifier("f".to_string()),
            },
        ),
    );

    assert_eq!(
        parser.logs,
        Vec::new(),
    );
}
