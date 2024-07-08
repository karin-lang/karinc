use crate::{id_token, keyword_token, literal_token, prim_type_token, token};
use crate::lexer::token::{self, Span};
use crate::parser::{Parser, ParserHakoContext};
use crate::parser::ast::*;
use crate::parser::log::ParserLog;
use crate::hir::id::*;

/* integrated */

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
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);
    let (ast, logs) = parser.parse(ModId::new(0, 0), "myhako".into());

    assert_eq!(
        ast,
        Ast {
            mod_id: ModId::new(0, 0),
            mod_path: "myhako".into(),
            items: vec![
                Item {
                    id: ItemId::new(0, 0),
                    name: Id {
                        id: "f".to_string(),
                        span: Span::new(1, 1),
                    },
                    accessibility: Accessibility::Default,
                    kind: ItemKind::FnDecl(
                        FnDecl {
                            body: Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                exprs: Vec::new(),
                            },
                        },
                    ),
                },
            ],
        },
    );
    assert!(logs.is_empty());
}

#[test]
fn parses_continuous_items() {
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
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_items(),
        vec![
            Item {
                id: ItemId::new(0, 0),
                name: Id {
                    id: "f1".to_string(),
                    span: Span::new(1, 1),
                },
                accessibility: Accessibility::Default,
                kind: ItemKind::FnDecl(
                    FnDecl {
                        body: Body {
                            id: BodyId::new(0),
                            ret_type: None,
                            args: Vec::new(),
                            exprs: Vec::new(),
                        },
                    },
                ),
            },
            Item {
                id: ItemId::new(0, 1),
                name: Id {
                    id: "f2".to_string(),
                    span: Span::new(7, 1),
                },
                accessibility: Accessibility::Default,
                kind: ItemKind::FnDecl(
                    FnDecl {
                        body: Body {
                            id: BodyId::new(1),
                            ret_type: None,
                            args: Vec::new(),
                            exprs: Vec::new(),
                        },
                    },
                ),
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn consumes_until_body_end_when_error_occurred() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        token!(OpenParen, 1, 1),
        token!(ClosingParen, 2, 1),
        token!(OpenCurlyBracket, 3, 1),
        token!(ClosingCurlyBracket, 4, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_items(),
        Vec::new(),
    );
    assert_eq!(
        *parser.get_logs(),
        vec![ParserLog::ExpectedId { span: Span::new(1, 1) }],
    );
    assert!(parser.peek().is_none());
}

/* body or block */

#[test]
fn parses_empty_body_or_block() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 1),
        token!(ClosingCurlyBracket, 1, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_body_or_block().unwrap(),
        Vec::new(),
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_body_or_block_with_single_expr() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 1),
        id_token!("id", 1, 1),
        token!(Semicolon, 2, 1),
        token!(ClosingCurlyBracket, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_body_or_block().unwrap(),
        vec![
            Expr {
                kind: ExprKind::Id(Id { id: "id".to_string(), span: Span::new(1, 1) }),
                span: Span::new(1, 1),
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_body_or_block_with_multiple_exprs() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 1),
        id_token!("id1", 1, 1),
        token!(Semicolon, 2, 1),
        id_token!("id2", 3, 1),
        token!(Semicolon, 4, 1),
        token!(ClosingCurlyBracket, 5, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_body_or_block().unwrap(),
        vec![
            Expr {
                kind: ExprKind::Id(Id { id: "id1".to_string(), span: Span::new(1, 1) }),
                span: Span::new(1, 1),
            },
            Expr {
                kind: ExprKind::Id(Id { id: "id2".to_string(), span: Span::new(3, 1) }),
                span: Span::new(3, 1),
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn expects_semicolon_after_expr_in_body_or_block() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 1),
        id_token!("id1", 1, 1),
        id_token!("id2",  2, 1),
        token!(ClosingCurlyBracket, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_body_or_block().unwrap(),
        vec![
            Expr {
                kind: ExprKind::Id(Id { id: "id1".to_string(), span: Span::new(1, 1) }),
                span: Span::new(1, 1),
            },
            Expr {
                kind: ExprKind::Id(Id { id: "id2".to_string(), span: Span::new(2, 1) }),
                span: Span::new(2, 1),
            },
        ],
    );
    assert_eq!(
        *parser.get_logs(),
        vec![
            ParserLog::ExpectedToken { kind: token::TokenKind::Semicolon, span: Span::new(2, 1) },
            ParserLog::ExpectedToken { kind: token::TokenKind::Semicolon, span: Span::new(3, 1) },
        ],
    );
    assert!(parser.peek().is_none());
}

/* identifier and path */

#[test]
fn parses_id_expr() {
    let tokens = vec![
        id_token!("id", 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::Id(
                Id { id: "id".to_string(), span: Span::new(0, 1) },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_path_expr() {
    let tokens = vec![
        id_token!("seg1", 0, 1),
        token!(DoubleColon, 1, 1),
        id_token!("seg2", 2, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::Path("seg1::seg2".into()),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* return */

#[test]
fn parses_ret_expr() {
    let tokens = vec![
        keyword_token!(Ret, 0, 1),
        id_token!("id", 1, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::Ret(
                Ret {
                    value: Box::new(
                        Expr {
                            kind: ExprKind::Id(Id { id: "id".to_string(), span: Span::new(1, 1) }),
                            span: Span::new(1, 1),
                        },
                    ),
                },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* function call */

#[test]
fn parses_fn_call_expr() {
    let tokens = vec![
        id_token!("f", 0, 1),
        token!(OpenParen, 1, 1),
        id_token!("a", 2, 1),
        token!(ClosingParen, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::FnCall(
                FnCall {
                    path: "f".into(),
                    args: vec![
                        ActualArg {
                            ref_mut: RefMut::None,
                            expr: Expr {
                                kind: ExprKind::Id(
                                    Id { id: "a".to_string(), span: Span::new(2, 1) },
                                ),
                                span: Span::new(2, 1),
                            },
                        },
                    ],
                },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_fn_call_expr_with_path() {
    let tokens = vec![
        id_token!("seg", 0, 1),
        token!(DoubleColon, 1, 1),
        id_token!("f", 2, 1),
        token!(OpenParen, 3, 1),
        token!(ClosingParen, 4, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::FnCall(
                FnCall {
                    path: "seg::f".into(),
                    args: Vec::new(),
                },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* actual argument */

#[test]
fn parses_actual_args_of_zero_len() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        token!(ClosingParen, 1, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert!(parser.parse_actual_args().unwrap().is_empty());
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_actual_arg_of_a_len() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        id_token!("a", 1, 1),
        token!(ClosingParen, 2, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_actual_args().unwrap(),
        vec![
            ActualArg {
                ref_mut: RefMut::None,
                expr: Expr {
                    kind: ExprKind::Id(
                        Id { id: "a".to_string(), span: Span::new(1, 1) },
                    ),
                    span: Span::new(1, 1),
                },
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_actual_args_of_two_len() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        id_token!("a1", 1, 1),
        token!(Comma, 2, 1),
        id_token!("a2", 3, 1),
        token!(ClosingParen, 4, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_actual_args().unwrap(),
        vec![
            ActualArg {
                ref_mut: RefMut::None,
                expr: Expr {
                    kind: ExprKind::Id(
                        Id { id: "a1".to_string(), span: Span::new(1, 1) },
                    ),
                    span: Span::new(1, 1),
                },
            },
            ActualArg {
                ref_mut: RefMut::None,
                expr: Expr {
                    kind: ExprKind::Id(
                        Id { id: "a2".to_string(), span: Span::new(3, 1) },
                    ),
                    span: Span::new(3, 1),
                },
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn disallows_comma_before_actual_arg() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        token!(Comma, 1, 1),
        id_token!("a", 2, 1),
        token!(ClosingParen, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_actual_args().unwrap(),
        vec![
            ActualArg {
                ref_mut: RefMut::None,
                expr: Expr {
                    kind: ExprKind::Id(
                        Id { id: "a".to_string(), span: Span::new(2, 1) },
                    ),
                    span: Span::new(2, 1),
                },
            },
        ],
    );
    assert_eq!(
        *parser.get_logs(),
        vec![ParserLog::ExpectedActualArg { span: Span::new(1, 1) }],
    );
    assert!(parser.peek().is_none());
}

#[test]
fn allows_comma_after_actual_arg() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        id_token!("a", 1, 1),
        token!(Comma, 2, 1),
        token!(ClosingParen, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_actual_args().unwrap(),
        vec![
            ActualArg {
                ref_mut: RefMut::None,
                expr: Expr {
                    kind: ExprKind::Id(
                        Id { id: "a".to_string(), span: Span::new(1, 1) },
                    ),
                    span: Span::new(1, 1),
                },
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_refmut_actual_arg() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        keyword_token!(Ref, 1, 1),
        id_token!("a", 2, 1),
        token!(ClosingParen, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_actual_args().unwrap(),
        vec![
            ActualArg {
                ref_mut: RefMut::Ref,
                expr: Expr {
                    kind: ExprKind::Id(
                        Id { id: "a".to_string(), span: Span::new(2, 1) },
                    ),
                    span: Span::new(2, 1),
                },
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

// #[test]
// fn expects_items_and_skips_line() {
//     let tokens = vec![
//         token!(Semicolon, 0, 1),
//         token!(Semicolon, 1, 1),
//         token!(Semicolon, 1, 0, 1),
//     ];
//     let mut parser = Parser::new(&tokens);

//     assert!(parser.parse_items().is_empty());
//     assert_eq!(
//         *parser.get_logs(),
//         vec![
//             ParserLog::ExpectedItem { span: Span::new(0, 0, 1) },
//             ParserLog::ExpectedItem { span: Span::new(1, 0, 1) },
//         ],
//     );
//     assert!(parser.peek().is_none());
// }

/* item */

#[test]
fn expects_item() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_single_item(),
        Err(ParserLog::ExpectedItem { span: Span::new(0, 1) }),
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(
        parser.peek(),
        Some(&&keyword_token!(Let,0,1)),
    );
}

/* item - function declaration */

#[test]
fn parses_fn_decl_item() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        id_token!("f", 1, 1),
        token!(OpenParen, 2, 1),
        token!(ClosingParen, 3, 1),
        token!(OpenCurlyBracket, 4, 1),
        token!(ClosingCurlyBracket, 5, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_single_item().unwrap(),
        Item {
            id: ItemId::new(0, 0),
            name: Id { id: "f".to_string(), span: Span::new(1, 1) },
            accessibility: Accessibility::Default,
            kind: ItemKind::FnDecl(
                FnDecl {
                    body: Body {
                        id: BodyId::new(0),
                        ret_type: None,
                        args: Vec::new(),
                        exprs: Vec::new(),
                    },
                },
            ),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_pub_fn_decl_item() {
    let tokens = vec![
        keyword_token!(Pub, 0, 1),
        keyword_token!(Fn, 1, 1),
        id_token!("f", 2, 1),
        token!(OpenParen, 3, 1),
        token!(ClosingParen, 4, 1),
        token!(OpenCurlyBracket, 5, 1),
        token!(ClosingCurlyBracket, 6, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_single_item().unwrap(),
        Item {
            id: ItemId::new(0, 0),
            name: Id { id: "f".to_string(), span: Span::new(2, 1) },
            accessibility: Accessibility::Pub,
            kind: ItemKind::FnDecl(
                FnDecl {
                    body: Body {
                        id: BodyId::new(0),
                        ret_type: None,
                        args: Vec::new(),
                        exprs: Vec::new(),
                    },
                },
            ),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_fn_decl_item_with_ret_type() {
    let tokens = vec![
        keyword_token!(Fn, 0, 1),
        id_token!("f", 1, 1),
        token!(OpenParen, 2, 1),
        token!(ClosingParen, 5, 1),
        prim_type_token!(Usize, 4, 1),
        token!(OpenCurlyBracket, 6, 1),
        token!(ClosingCurlyBracket, 7, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_single_item().unwrap(),
        Item {
            id: ItemId::new(0, 0),
            name: Id { id: "f".to_string(), span: Span::new(1, 1) },
            accessibility: Accessibility::Default,
            kind: ItemKind::FnDecl(
                FnDecl {
                    body: Body {
                        id: BodyId::new(0),
                        ret_type: Some(
                            Type {
                                kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                                span: Span::new(4, 1),
                            },
                        ),
                        args: Vec::new(),
                        exprs: Vec::new(),
                    },
                },
            ),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* referability and mutability */

#[test]
fn parses_referable() {
    let tokens = vec![
        keyword_token!(Ref, 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.consume_ref_mut(),
        RefMut::Ref,
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_mutability() {
    let tokens = vec![
        keyword_token!(Mut, 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.consume_ref_mut(),
        RefMut::Mut,
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_none_refmut() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.consume_ref_mut(),
        RefMut::None,
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(parser.peek(), Some(&&keyword_token!(Let, 0, 1)));
}

/* formal argument */

#[test]
fn parses_formal_args_of_zero_len() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        token!(ClosingParen, 1, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_formal_args().unwrap(),
        Vec::new(),
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_formal_arg_of_a_len() {
    let tokens = vec![
        token!(OpenParen,  0, 1),
        id_token!("a", 1, 1),
        prim_type_token!(Usize, 2, 1),
        token!(ClosingParen, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_formal_args().unwrap(),
        vec![
            FormalArg {
                id: Id { id: "a".to_string(), span: Span::new(1, 1) },
                r#type: Type {
                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                    span: Span::new(2, 1),
                },
                ref_mut: RefMut::None,
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_formal_args_of_two_len() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        id_token!("a1", 2, 1),
        prim_type_token!(Usize, 3, 1),
        token!(Comma, 4, 1),
        id_token!("a2", 5, 1),
        prim_type_token!(Usize, 6, 1),
        token!(ClosingParen, 7, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_formal_args().unwrap(),
        vec![
            FormalArg {
                id: Id { id: "a1".to_string(), span: Span::new(2, 1) },
                r#type: Type {
                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                    span: Span::new(3, 1),
                },
                ref_mut: RefMut::None,
            },
            FormalArg {
                id: Id { id: "a2".to_string(), span: Span::new(5, 1) },
                r#type: Type {
                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                    span: Span::new(6, 1),
                },
                ref_mut: RefMut::None,
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_refmut_of_formal_arg() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        id_token!("a", 1, 1),
        keyword_token!(Mut, 2, 1),
        prim_type_token!(Usize, 3, 1),
        token!(ClosingParen, 4, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_formal_args().unwrap(),
        vec![
            FormalArg {
                id: Id { id: "a".to_string(), span: Span::new(1, 1) },
                r#type: Type {
                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                    span: Span::new(3, 1),
                },
                ref_mut: RefMut::Mut,
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn disallows_comma_before_formal_args() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        token!(Comma, 1, 1),
        id_token!("a", 2, 1),
        prim_type_token!(Usize, 3, 1),
        token!(ClosingParen, 4, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_formal_args().unwrap(),
        vec![
            FormalArg {
                id: Id { id: "a".to_string(), span: Span::new(2, 1) },
                r#type: Type {
                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                    span: Span::new(3, 1),
                },
                ref_mut: RefMut::None,
            },
        ],
    );
    assert_eq!(
        *parser.get_logs(),
        vec![ParserLog::ExpectedFormalArg { span: Span::new(1, 1) }],
    );
    assert!(parser.peek().is_none());
}

#[test]
fn allows_comma_after_formal_args() {
    let tokens = vec![
        token!(OpenParen, 0, 1),
        id_token!("a", 1, 1),
        prim_type_token!(Usize, 2, 1),
        token!(Comma, 3, 1),
        token!(ClosingParen, 4, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_formal_args().unwrap(),
        vec![
            FormalArg {
                id: Id { id: "a".to_string(), span: Span::new(1, 1) },
                r#type: Type {
                    kind: Box::new(TypeKind::Prim(PrimType::Usize)),
                    span: Span::new(2, 1),
                },
                ref_mut: RefMut::None,
            },
        ],
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* body */

#[test]
fn parses_body() {
    let tokens = vec![
        token!(OpenCurlyBracket, 0, 1),
        id_token!("id", 1, 1),
        token!(Semicolon, 2, 1),
        token!(ClosingCurlyBracket, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_body(None, Vec::new()).unwrap(),
        Body {
            id: BodyId::new(0),
            ret_type: None,
            args: Vec::new(),
            exprs: vec![
                Expr {
                    kind: ExprKind::Id(Id { id: "id".to_string(), span: Span::new(1, 1) }),
                    span: Span::new(1, 1),
                },
            ],
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* type */

#[test]
fn parses_id_type() {
    let tokens = vec![
        id_token!("t", 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_type().unwrap(),
        Type {
            kind: Box::new(
                TypeKind::Id(
                    Id { id: "t".to_string(), span: Span::new(0, 1) },
                ),
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_prim_type() {
    let tokens = vec![
        prim_type_token!(Bool, 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_type().unwrap(),
        Type {
            kind: Box::new(TypeKind::Prim(PrimType::Bool)),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn expects_type_for_unexpected_token() {
    let tokens = vec![
        token!(Semicolon, 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_type(),
        Err(ParserLog::ExpectedType { span: Span::new(0, 1) })
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* expression */

#[test]
fn consumes_until_before_semicolon_when_error_occurred() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
        token!(Semicolon, 1, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr(),
        Err(ParserLog::ExpectedId { span: Span::new(1, 1) }),
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(parser.peek(), Some(&&token!(Semicolon, 1, 1)));
}

/* variable definition */

#[test]
fn parses_var_def_expr() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
        id_token!("i", 1, 1),
        token!(Semicolon, 2, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr(),
        Ok(
            Expr {
                kind: ExprKind::VarDef(
                    VarDef {
                        id: Id { id: "i".to_string(), span: Span::new(1, 1) },
                        ref_mut: RefMut::None,
                        r#type: None,
                        init: None,
                    },
                ),
                span: Span::new(1, 1),
            },
        ),
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(parser.peek(), Some(&&token!(Semicolon, 2, 1)));
}

#[test]
fn parses_refmut_var_def() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
        keyword_token!(Ref, 1, 1),
        id_token!("i", 2, 1),
        token!(Semicolon, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_var_def().unwrap(),
        (
            VarDef {
                id: Id { id: "i".to_string(), span: Span::new(2, 1) },
                ref_mut: RefMut::Ref,
                r#type: None,
                init: None,
            },
            Span::new(2, 1),
        ),
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(parser.peek(), Some(&&token!(Semicolon, 3, 1)));
}

#[test]
fn parses_var_def_with_type_annot() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
        id_token!("i", 1, 1),
        prim_type_token!(Bool, 2, 1),
        token!(Semicolon, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_var_def().unwrap(),
        (
            VarDef {
                id: Id { id: "i".to_string(), span: Span::new(1, 1) },
                ref_mut: RefMut::None,
                r#type: Some(
                    Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Bool)),
                        span: Span::new(2, 1),
                    },
                ),
                init: None,
            },
            Span::new(1, 1),
        ),
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(parser.peek(), Some(&&token!(Semicolon, 3, 1)));
}

#[test]
fn parses_var_def_with_init() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
        id_token!("i", 1, 1),
        token!(Equal, 2, 1),
        id_token!("init", 3, 1),
        token!(Semicolon, 4, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_var_def().unwrap(),
        (
            VarDef {
                id: Id { id: "i".to_string(), span: Span::new(1, 1) },
                ref_mut: RefMut::None,
                r#type: None,
                init: Some(
                    Box::new(
                        Expr {
                            kind: ExprKind::Id(
                                Id { id: "init".to_string(), span: Span::new(3, 1) },
                            ),
                            span: Span::new(3, 1),
                        },
                    ),
                ),
            },
            Span::new(1, 1),
        ),
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(parser.peek(), Some(&&token!(Semicolon, 4, 1)));
}

#[test]
fn parses_var_def_with_type_annot_and_init() {
    let tokens = vec![
        keyword_token!(Let, 0, 1),
        id_token!("i", 1, 1),
        prim_type_token!(Bool, 2, 1),
        token!(Equal, 3, 1),
        id_token!("init", 4, 1),
        token!(Semicolon, 5, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_var_def().unwrap(),
        (
            VarDef {
                id: Id { id: "i".to_string(), span: Span::new(1, 1) },
                ref_mut: RefMut::None,
                r#type: Some(
                    Type {
                        kind: Box::new(TypeKind::Prim(PrimType::Bool)),
                        span: Span::new(2, 1),
                    },
                ),
                init: Some(
                    Box::new(
                        Expr {
                            kind: ExprKind::Id(
                                Id { id: "init".to_string(), span: Span::new(4, 1) },
                            ),
                            span: Span::new(4, 1),
                        },
                    ),
                ),
            },
            Span::new(1, 1),
        ),
    );
    assert!(parser.get_logs().is_empty());
    assert_eq!(parser.peek(), Some(&&token!(Semicolon, 5, 1)));
}

/* variable binding */

#[test]
fn parses_var_bind_expr() {
    let tokens = vec![
        id_token!("i", 0, 1),
        token!(Equal, 1, 1),
        id_token!("value", 2, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::VarBind(
                VarBind {
                    id: Id {
                        id: "i".to_string(),
                        span: Span::new(0, 1),
                    },
                    value: Box::new(
                        Expr {
                            kind: ExprKind::Id(
                                Id {
                                    id: "value".to_string(),
                                    span: Span::new(2, 1),
                                },
                            ),
                            span: Span::new(2, 1),
                        },
                    ),
                },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* if */

#[test]
fn parses_if_expr() {
    let tokens = vec![
        // if cond {}
        keyword_token!(If, 0, 1),
        id_token!("cond", 1, 1),
        token!(OpenCurlyBracket, 2, 1),
        token!(ClosingCurlyBracket, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::If(
                If {
                    cond: Box::new(
                        Expr {
                            kind: ExprKind::Id(
                                Id { id: "cond".to_string(), span: Span::new(1, 1) }
                            ),
                            span: Span::new(1, 1),
                        },
                    ),
                    block: Block {
                        exprs: Vec::new(),
                    },
                    elifs: Vec::new(),
                    r#else: None,
                },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_if_with_elifs_and_else() {
    let tokens = vec![
        // if cond {} elif cond {} elif cond {} else {}
        keyword_token!(If, 0, 1),
        id_token!("cond", 1, 1),
        token!(OpenCurlyBracket, 2, 1),
        token!(ClosingCurlyBracket, 3, 1),

        keyword_token!(Elif, 4, 1),
        id_token!("cond", 5, 1),
        token!(OpenCurlyBracket, 6, 1),
        token!(ClosingCurlyBracket, 7, 1),

        keyword_token!(Elif, 8, 1),
        id_token!("cond", 9, 1),
        token!(OpenCurlyBracket, 10, 1),
        token!(ClosingCurlyBracket, 11, 1),

        keyword_token!(Else, 12, 1),
        token!(OpenCurlyBracket, 13, 1),
        token!(ClosingCurlyBracket, 14, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_if().unwrap(),
        If {
            cond: Box::new(
                Expr {
                    kind: ExprKind::Id(
                        Id { id: "cond".to_string(), span: Span::new(1, 1) }
                    ),
                    span: Span::new(1, 1),
                },
            ),
            block: Block {
                exprs: Vec::new(),
            },
            elifs: vec![
                Elif {
                    cond: Box::new(
                        Expr {
                            kind: ExprKind::Id(
                                Id { id: "cond".to_string(), span: Span::new(5, 1) }
                            ),
                            span: Span::new(5, 1),
                        },
                    ),
                    block: Block {
                        exprs: Vec::new(),
                    },
                },
                Elif {
                    cond: Box::new(
                        Expr {
                            kind: ExprKind::Id(
                                Id { id: "cond".to_string(), span: Span::new(9, 1) }
                            ),
                            span: Span::new(9, 1),
                        },
                    ),
                    block: Block {
                        exprs: Vec::new(),
                    },
                },
            ],
            r#else: Some(
                Block {
                    exprs: Vec::new(),
                },
            ),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* for */

#[test]
fn parses_for_expr() {
    let tokens = vec![
        // for {}
        keyword_token!(For, 0, 1),
        token!(OpenCurlyBracket, 1, 1),
        token!(ClosingCurlyBracket, 2, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::For(
                For {
                    kind: ForKind::Endless,
                    block: Block {
                        exprs: Vec::new(),
                    },
                },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_for_with_cond() {
    let tokens = vec![
        // for cond {}
        keyword_token!(For, 0, 1),
        id_token!("cond", 1, 1),
        token!(OpenCurlyBracket, 2, 1),
        token!(ClosingCurlyBracket, 3, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_for().unwrap(),
        For {
            kind: ForKind::Cond {
                cond: Box::new(
                    Expr {
                        kind: ExprKind::Id(
                            Id { id: "cond".to_string(), span: Span::new(1, 1) }
                        ),
                        span: Span::new(1, 1),
                    },
                ),
            },
            block: Block {
                exprs: Vec::new(),
            },
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

#[test]
fn parses_for_in() {
    let tokens = vec![
        // for index in range {}
        keyword_token!(For, 0, 1),
        id_token!("index", 1, 1),
        keyword_token!(In, 2, 1),
        id_token!("range", 3, 1),
        token!(OpenCurlyBracket, 4, 1),
        token!(ClosingCurlyBracket, 5, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_for().unwrap(),
        For {
            kind: ForKind::Range {
                index: Box::new(
                    Expr {
                        kind: ExprKind::Id(
                            Id { id: "index".to_string(), span: Span::new(1, 1) }
                        ),
                        span: Span::new(1, 1),
                    },
                ),
                range: Box::new(
                    Expr {
                        kind: ExprKind::Id(
                            Id { id: "range".to_string(), span: Span::new(3, 1) }
                        ),
                        span: Span::new(3, 1),
                    },
                ),
            },
            block: Block {
                exprs: Vec::new(),
            },
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}

/* literal */

#[test]
fn parses_literal_expr() {
    let tokens = vec![
        literal_token!(token::Literal::Bool { value: true }, 0, 1),
    ];
    let mut crate_context = ParserHakoContext::new(HakoId::new(0));
    let mut last_body_id = 0;
    let mut parser = Parser::new(&tokens, &mut crate_context, &mut last_body_id);

    assert_eq!(
        parser.parse_expr().unwrap(),
        Expr {
            kind: ExprKind::Literal(
                token::Literal::Bool { value: true },
            ),
            span: Span::new(0, 1),
        },
    );
    assert!(parser.get_logs().is_empty());
    assert!(parser.peek().is_none());
}
