use crate::{id_token, keyword_token, token};
use crate::parser::{ast::*, Parser, ParserLog};

// todo: テストを追加

#[test]
fn outputs_parser_result() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        id_token!("f", 1, 1),
        token!(OpenParen, 2, 1),
        token!(ClosingParen, 3, 1),
        token!(OpenCurlyBracket, 4, 1),
        token!(ClosingCurlyBracket, 5, 1),
    ];
    let parser = Parser::new(&tokens);
    let (ast, logs) = parser.parse();

    assert_eq!(
        ast,
        Ast {
            items: vec![
                Item {
                    kind: Box::new(
                        ItemKind::FnDecl(
                            FnDecl {
                                id: "f".into(),
                                args: Vec::new(),
                                ret_type: None,
                                body: Vec::new(),
                            },
                        ),
                    ),
                },
            ],
        },
    );
    assert_eq!(logs, Vec::new());
}

#[test]
fn parses_single_item() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        id_token!("f", 1, 1),
        token!(OpenParen, 2, 1),
        token!(ClosingParen, 3, 1),
        token!(OpenCurlyBracket, 4, 1),
        token!(ClosingCurlyBracket, 5, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        vec![
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: "f".into(),
                            args: Vec::new(),
                            ret_type: None,
                            body: Vec::new(),
                        },
                    ),
                ),
            },
        ],
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_multiple_items() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        id_token!("f1", 1, 1),
        token!(OpenParen, 2, 1),
        token!(ClosingParen, 3, 1),
        token!(OpenCurlyBracket, 4, 1),
        token!(ClosingCurlyBracket, 5, 1),
        keyword_token!(Fn, 6, 1),
        id_token!("f2", 7, 1),
        token!(OpenParen, 8, 1),
        token!(ClosingParen, 9, 1),
        token!(OpenCurlyBracket, 10, 1),
        token!(ClosingCurlyBracket, 11, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        vec![
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: "f1".into(),
                            args: Vec::new(),
                            ret_type: None,
                            body: Vec::new(),
                        },
                    ),
                ),
            },
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: "f2".into(),
                            args: Vec::new(),
                            ret_type: None,
                            body: Vec::new(),
                        },
                    ),
                ),
            },
        ],
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn records_item_parsing_log() {
    let tokens = vec![token!(Semicolon, 0, 1)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        Vec::new(),
    );
    assert_eq!(
        *parser.get_logs(),
        vec![ParserLog::ExpectedItem],
    );
    assert!(parser.peek().is_none());
}

// todo: parse_items() のエラー回復テストを追加する

#[test]
fn parses_fn_decl() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        id_token!("f", 1, 1),
        token!(OpenParen, 2, 1),
        token!(ClosingParen, 3, 1),
        token!(OpenCurlyBracket, 4, 1),
        token!(ClosingCurlyBracket, 5, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_single_item(),
        Ok(
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: "f".into(),
                            args: Vec::new(),
                            ret_type: None,
                            body: Vec::new(),
                        },
                    ),
                ),
            },
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_fn_decl_with_ret_type() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        id_token!("f", 1, 1),
        token!(OpenParen, 2, 1),
        token!(ClosingParen, 5, 1),
        keyword_token!(Usize, 4, 1),
        token!(OpenCurlyBracket, 6, 1),
        token!(ClosingCurlyBracket, 7, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_single_item(),
        Ok(
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: "f".into(),
                            args: Vec::new(),
                            ret_type: Some(
                                Type {
                                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                                },
                            ),
                            body: Vec::new(),
                        },
                    ),
                ),
            },
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}
