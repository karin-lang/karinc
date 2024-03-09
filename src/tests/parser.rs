use crate::{id_token, keyword_token, token};
use crate::lexer::token::{Span, TokenKind};
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
fn parses_continuous_items() {
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
fn expects_items_and_skips_line() {
    let tokens = vec![
        token!(Semicolon, 0, 0, 1),
        token!(Semicolon, 0, 1, 1),
        token!(Semicolon, 1, 0, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_items(),
        Vec::new(),
    );
    assert_eq!(
        *parser.get_logs(),
        vec![
            ParserLog::ExpectedItem { span: Span::new(0, 0, 1) },
            ParserLog::ExpectedItem { span: Span::new(1, 0, 1) },
        ],
    );
    assert!(parser.peek().is_none());
}

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

#[test]
fn parses_formal_args_of_zero_len() {
    let tokens = vec![
        token!(OpenParen, 0, 0, 1),
        token!(ClosingParen, 0, 1, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_formal_args(),
        Ok(Vec::new()),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_formal_arg_of_a_len() {
    let tokens = vec![
        token!(OpenParen, 0, 0, 1),
        id_token!("a", 0, 1, 1),
        keyword_token!(Usize, 0, 2, 1),
        token!(ClosingParen, 0, 3, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_formal_args(),
        Ok(
            vec![
                FormalArg {
                    id: Id { id: "a".to_string(), span: Span::new(0, 1, 1) },
                    r#type: Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                        span: Span::new(0, 2, 1),
                    },
                    mutable: false,
                },
            ],
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_formal_args_of_two_len() {
    let tokens = vec![
        token!(OpenParen, 0, 0, 1),
        id_token!("a1", 0, 2, 1),
        keyword_token!(Usize, 0, 3, 1),
        token!(Comma, 0, 4, 1),
        id_token!("a2", 0, 5, 1),
        keyword_token!(Usize, 0, 6, 1),
        token!(ClosingParen, 0, 7, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_formal_args(),
        Ok(
            vec![
                FormalArg {
                    id: Id { id: "a1".to_string(), span: Span::new(0, 2, 1) },
                    r#type: Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                        span: Span::new(0, 3, 1),
                    },
                    mutable: false,
                },
                FormalArg {
                    id: Id { id: "a2".to_string(), span: Span::new(0, 5, 1) },
                    r#type: Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                        span: Span::new(0, 6, 1),
                    },
                    mutable: false,
                },
            ],
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_mutable_formal_arg() {
    let tokens = vec![
        token!(OpenParen, 0, 0, 1),
        id_token!("a", 0, 1, 1),
        keyword_token!(Mut, 0, 2, 1),
        keyword_token!(Usize, 0, 3, 1),
        token!(ClosingParen, 0, 4, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_formal_args(),
        Ok(
            vec![
                FormalArg {
                    id: Id { id: "a".to_string(), span: Span::new(0, 1, 1) },
                    r#type: Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                        span: Span::new(0, 3, 1),
                    },
                    mutable: true,
                },
            ],
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn disallows_comma_before_formal_args() {
    let tokens = vec![
        token!(OpenParen, 0, 0, 1),
        token!(Comma, 0, 1, 1),
        id_token!("a", 0, 2, 1),
        keyword_token!(Usize, 0, 3, 1),
        token!(ClosingParen, 0, 4, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_formal_args(),
        Ok(
            vec![
                FormalArg {
                    id: Id { id: "a".to_string(), span: Span::new(0, 2, 1) },
                    r#type: Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                        span: Span::new(0, 3, 1),
                    },
                    mutable: false,
                },
            ],
        ),
    );
    assert_eq!(
        *parser.get_logs(),
        vec![ParserLog::ExpectedFormalArg { span: Span::new(0, 1, 1) }],
    );
    assert!(parser.peek().is_none());
}

#[test]
fn allows_comma_after_formal_args() {
    let tokens = vec![
        token!(OpenParen, 0, 0, 1),
        id_token!("a", 0, 1, 1),
        keyword_token!(Usize, 0, 2, 1),
        token!(Comma, 0, 3, 1),
        token!(ClosingParen, 0, 4, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_formal_args(),
        Ok(
            vec![
                FormalArg {
                    id: Id { id: "a".to_string(), span: Span::new(0, 1, 1) },
                    r#type: Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                        span: Span::new(0, 2, 1),
                    },
                    mutable: false,
                },
            ],
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_empty_body() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 0, 1),
        token!(ClosingCurlyBracket, 0, 1, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_body(),
        Ok(Vec::new()),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_body_with_single_expr() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 0, 1),
        id_token!("id", 0, 1, 1),
        token!(Semicolon, 0, 2, 1),
        token!(ClosingCurlyBracket, 0, 3, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_body(),
        Ok(
            vec![
                Expr {
                    kind: Box::new(ExprKind::Id(Id { id: "id".to_string(), span: Span::new(0, 1, 1) })),
                    span: Span::new(0, 1, 1),
                },
            ],
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_body_with_multiple_exprs() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 0, 1),
        id_token!("id1", 0, 1, 1),
        token!(Semicolon, 0, 2, 1),
        id_token!("id2", 0, 3, 1),
        token!(Semicolon, 0, 4, 1),
        token!(ClosingCurlyBracket, 0, 5, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_body(),
        Ok(
            vec![
                Expr {
                    kind: Box::new(ExprKind::Id(Id { id: "id1".to_string(), span: Span::new(0, 1, 1) })),
                    span: Span::new(0, 1, 1),
                },
                Expr {
                    kind: Box::new(ExprKind::Id(Id { id: "id2".to_string(), span: Span::new(0, 3, 1) })),
                    span: Span::new(0, 3, 1),
                },
            ],
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn expects_semicolon_after_expr_in_body() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 0, 1),
        id_token!("id1", 0, 1, 1),
        id_token!("id2", 0, 2, 1),
        token!(ClosingCurlyBracket, 0, 3, 1),
    ];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_body(),
        Ok(
            vec![
                Expr {
                    kind: Box::new(ExprKind::Id(Id { id: "id1".to_string(), span: Span::new(0, 1, 1) })),
                    span: Span::new(0, 1, 1),
                },
                Expr {
                    kind: Box::new(ExprKind::Id(Id { id: "id2".to_string(), span: Span::new(0, 2, 1) })),
                    span: Span::new(0, 2, 1),
                },
            ],
        ),
    );
    assert_eq!(
        *parser.get_logs(),
        vec![
            ParserLog::ExpectedToken { kind: TokenKind::Semicolon, span: Span::new(0, 2, 1) },
            ParserLog::ExpectedToken { kind: TokenKind::Semicolon, span: Span::new(0, 3, 1) },
        ],
    );
    assert!(parser.peek().is_none());
}

#[test]
fn parses_id_type() {
    let tokens = vec![id_token!("t", 0, 0, 1)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_type(),
        Ok(
            Type {
                kind: Box::new(
                    TypeKind::Id(
                        Id { id: "t".to_string(), span: Span::new(0, 0, 1) },
                    ),
                ),
                span: Span::new(0, 0, 1),
            },
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_prim_type() {
    let tokens = vec![keyword_token!(Usize, 0, 0, 1)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_type(),
        Ok(
            Type {
                kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                span: Span::new(0, 0, 1),
            },
        ),
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn expects_type_for_unexpected_token() {
    let tokens = vec![token!(Semicolon, 0, 0, 1)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_type(),
        Err(ParserLog::ExpectedType { span: Span::new(0, 0, 1) })
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}

#[test]
fn expectes_type_for_unexpected_keyword() {
    let tokens = vec![keyword_token!(Pub, 0, 0, 1)];
    let mut parser = Parser::new(&tokens);

    assert_eq!(
        parser.parse_type(),
        Err(ParserLog::ExpectedType { span: Span::new(0, 0, 1) })
    );
    assert_eq!(*parser.get_logs(), Vec::new());
    assert!(parser.peek().is_none());
}
