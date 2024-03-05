use crate::{id_token, keyword_token, symbol_token};
use crate::parser::{ast::*, Parser};
use crate::lexer::token::*;

// todo: テストを追加

#[test]
fn outputs_parser_result() {
    let tokens = vec![
        keyword_token!(Function, 0, 1),
        id_token!("f", 1, 1),
        symbol_token!(OpenParen, 2, 1),
        symbol_token!(ClosingParen, 3, 1),
        symbol_token!(OpenCurlyBracket, 4, 1),
        symbol_token!(ClosingCurlyBracket, 5, 1),
    ];
    let parser = Parser::new(&tokens);
    let (ast, logs) = parser.parse();

    assert_eq!(
        ast,
        Ast {
            items: vec![
                Item {
                    kind: ItemKind::FnDecl(
                        FnDecl {
                            id: "f".into(),
                            args: Vec::new(),
                            body: Vec::new(),
                        },
                    ),
                },
            ],
        },
    );
    assert_eq!(logs, Vec::new());
}

#[test]
fn parses_items() {
    let tokens = vec![
        keyword_token!(Function, 0, 1),
        id_token!("f", 1, 1),
        symbol_token!(OpenParen, 2, 1),
        symbol_token!(ClosingParen, 3, 1),
        symbol_token!(OpenCurlyBracket, 4, 1),
        symbol_token!(ClosingCurlyBracket, 5, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        vec![
            Item {
                kind: ItemKind::FnDecl(
                    FnDecl {
                        id: "f".into(),
                        args: Vec::new(),
                        body: Vec::new(),
                    },
                ),
            },
        ],
    );
    assert_eq!(parser.get_logs(), &Vec::new());
}
