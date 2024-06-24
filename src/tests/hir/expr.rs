use std::collections::HashMap;

use log::HirLoweringLog;
use maplit::hashmap;

use crate::lexer::token::{self, Span};
use crate::parser::ast;
use crate::hir::*;
use crate::hir::id::*;
use crate::hir::lower::HirLowering;

#[test]
fn resolves_literal_expr() {
    let src = ast::Expr {
        kind: ast::ExprKind::Literal(
            token::Literal::Bool { value: true },
        ),
        span: Span::new(1, 1),
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_expr(&src);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::Literal(
                token::Literal::Bool { value: true },
            ),
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn resolves_fn_call_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::FnCall(
            ast::FnCall {
                path: "my_hako::f".into(),
                args: vec![
                    ast::ActualArg {
                        expr: ast::Expr {
                            kind: ast::ExprKind::Literal(
                                token::Literal::Bool { value: true },
                            ),
                            span: Span::new(1, 1),
                        },
                    },
                ],
            },
        ),
        span: Span::new(0, 1),
    };
    let asts = Vec::new();
    let paths = hashmap! {
        "my_hako::f".into() => GlobalId::Item(ItemId::new(0, 0)),
    };
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_expr(&ast);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::FnCall(
                FnCall {
                    r#fn: Some(ItemId::new(0,0)),
                    args: vec![
                        ActualArg {
                            expr: Expr {
                                id: ExprId::new(1),
                                kind: ExprKind::Literal(
                                    token::Literal::Bool { value: true },
                                ),
                            },
                        },
                    ],
                },
            ),
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn makes_fn_call_id_none_when_path_not_found() {
    let ast = ast::FnCall {
        path: "my_hako::f".into(),
        args: Vec::new(),
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_fn_call(&ast, Span::new(0, 1));

    assert_eq!(
        hir,
        FnCall {
            r#fn: None,
            args: Vec::new(),
        },
    );
    assert_eq!(
        *lowering.get_logs(),
        hashmap! {
            ModId::new(0, 0) => vec![
                HirLoweringLog::PathIsNotFoundInScope {
                    path: "my_hako::f".into(),
                    span: Span::new(0, 1),
                },
            ],
        },
    );
}
