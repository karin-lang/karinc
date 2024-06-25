use std::collections::HashMap;

use log::HirLoweringLog;
use maplit::hashmap;

use crate::lexer::token::{self, Span};
use crate::parser::ast;
use crate::hir::*;
use crate::hir::id::*;
use crate::hir::lower::HirLowering;

#[test]
fn lowers_literal_expr() {
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
fn lowers_fn_call_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::FnCall(
            ast::FnCall {
                path: "my_hako::f".into(),
                args: vec![
                    ast::ActualArg {
                        ref_mut: ast::RefMut::None,
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

#[test]
fn lowers_var_def_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::VarDef(
            ast::VarDef {
                id: ast::Id { id: "i".to_string(), span: Span::new(0, 1) },
                ref_mut: ast::RefMut::None,
                r#type: Some(
                    ast::Type {
                        kind: Box::new(ast::TypeKind::Prim(ast::PrimType::Bool)),
                        span: Span::new(1, 1),
                    },
                ),
                init: Some(
                    Box::new(
                        ast::Expr {
                            kind: ast::ExprKind::Literal(
                                token::Literal::Bool { value: true },
                            ),
                            span: Span::new(2, 1),
                        },
                    ),
                ),
            },
        ),
        span: Span::new(0, 1),
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_expr(&ast);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::VarDef(VarId::new(0)),
        },
    );
    assert_eq!(
        *lowering.get_body_scope_hierarchy().get_current_scope().get_vars(),
        vec![
            VarDef {
                ref_mut: ast::RefMut::None,
                r#type: Some(
                    Type {
                        kind: Box::new(TypeKind::Prim(ast::PrimType::Bool)),
                    },
                ),
                init: Some(
                    Expr {
                        id: ExprId::new(1),
                        kind: ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                    },
                ),
            },
        ],
    );
    assert!(lowering.get_logs().is_empty());
}
