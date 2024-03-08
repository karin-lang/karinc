use crate::{id_token, keyword_token, token};
use crate::lexer::token::Span;
use crate::parser::{ast::*, Parser, ParserLog};

// todo: テストを追加

#[test]
fn outputs_parser_result() {
    let tokens = vec![
        keyword_token!(Fn, 0, 0, 1),
        id_token!("f", 0, 1, 1),
        token!(OpenParen, 0, 2, 1),
        token!(ClosingParen, 0, 3, 1),
        token!(OpenCurlyBracket, 0, 4, 1),
        token!(ClosingCurlyBracket, 0, 5, 1),
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
                                id: Id {
                                    id: "f".to_string(),
                                    span: Span::new(0, 1, 1),
                                },
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
        keyword_token!(Fn, 0, 0, 1),
        id_token!("f", 0, 1, 1),
        token!(OpenParen, 0, 2, 1),
        token!(ClosingParen, 0, 3, 1),
        token!(OpenCurlyBracket, 0, 4, 1),
        token!(ClosingCurlyBracket, 0, 5, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        vec![
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: Id {
                                id: "f".to_string(),
                                span: Span::new(0, 1, 1),
                            },
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
        keyword_token!(Fn, 0, 0, 1),
        id_token!("f1", 0, 1, 1),
        token!(OpenParen, 0, 2, 1),
        token!(ClosingParen, 0, 3, 1),
        token!(OpenCurlyBracket, 0, 4, 1),
        token!(ClosingCurlyBracket, 0, 5, 1),
        keyword_token!(Fn, 0, 6, 1),
        id_token!("f2", 0, 7, 1),
        token!(OpenParen, 0, 8, 1),
        token!(ClosingParen, 0, 9, 1),
        token!(OpenCurlyBracket, 0, 10, 1),
        token!(ClosingCurlyBracket, 0, 11, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        vec![
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: Id {
                                id: "f1".to_string(),
                                span: Span::new(0, 1, 1),
                            },
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
                            id: Id {
                                id: "f2".to_string(),
                                span: Span::new(0, 7, 1),
                            },
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
    let tokens = vec![token!(Semicolon, 0, 0, 1)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        Vec::new(),
    );
    assert_eq!(
        *parser.get_logs(),
        vec![ParserLog::ExpectedItem { span: Span::new(0, 0, 1) }],
    );
    assert!(parser.peek().is_none());
}

// todo: parse_items() のエラー回復テストを追加する

#[test]
fn parses_fn_decl() {
    let tokens = vec![
        keyword_token!(Fn, 0, 0, 1),
        id_token!("f", 0, 1, 1),
        token!(OpenParen, 0, 2, 1),
        token!(ClosingParen, 0, 3, 1),
        token!(OpenCurlyBracket, 0, 4, 1),
        token!(ClosingCurlyBracket, 0, 5, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_single_item(),
        Ok(
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: Id {
                                id: "f".to_string(),
                                span: Span::new(0, 1, 1),
                            },
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
        keyword_token!(Fn, 0, 0, 1),
        id_token!("f", 0, 1, 1),
        token!(OpenParen, 0, 2, 1),
        token!(ClosingParen, 0, 5, 1),
        keyword_token!(Usize, 0, 4, 1),
        token!(OpenCurlyBracket, 0, 6, 1),
        token!(ClosingCurlyBracket, 0, 7, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_single_item(),
        Ok(
            Item {
                kind: Box::new(
                    ItemKind::FnDecl(
                        FnDecl {
                            id: Id {
                                id: "f".to_string(),
                                span: Span::new(0, 1, 1),
                            },
                            args: Vec::new(),
                            ret_type: Some(
                                Type {
                                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                                    span: Span::new(0, 4, 1),
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
