use std::collections::HashMap;

use maplit::hashmap;

use crate::hir::*;
use crate::hir::lower::HirLowering;
use crate::lexer::token::Span;
use crate::parser::ast;

#[test]
fn lowers_empty_ast() {
    let asts = vec![ast::Ast { mod_path: "myhako".into(), items: Vec::new() }];
    let lowering = HirLowering::new(&asts);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir { items: HashMap::new() },
    );
    assert!(logs.is_empty());
}

#[test]
fn lowers_subitem_in_mod() {
    let asts = vec![
        ast::Ast {
            mod_path: "myhako".into(),
            items: vec![
                ast::Item {
                    id: ast::Id { id: "f".to_string(), span: Span::new(0, 1) },
                    kind: ast::ItemKind::FnDecl(
                        ast::FnDecl {
                            args: Vec::new(),
                            ret_type: None,
                            body: ast::Body { exprs: Vec::new() },
                        },
                    ),
                },
            ],
        },
    ];
    let lowering = HirLowering::new(&asts);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir {
            items: hashmap! {
                "myhako::f".into() => (
                    Item::FnDecl(
                        FnDecl {
                            args: Vec::new(),
                            body: Body {
                                locals: Vec::new(),
                                exprs: Vec::new(),
                            },
                        },
                    )
                ),
            },
        },
    );
    assert!(logs.is_empty());
}

#[test]
fn resolves_item_and_local() {
    let ast = vec![
        ast::Ast {
            mod_path: "myhako".into(),
            items: vec![
                ast::Item {
                    id: ast::Id { id: "item".to_string(), span: Span::new(0, 1) },
                    kind: ast::ItemKind::FnDecl(
                        ast::FnDecl {
                            args: Vec::new(),
                            ret_type: None,
                            body: ast::Body {
                                exprs: vec![
                                    ast::Expr {
                                        kind: ast::ExprKind::Id(
                                            ast::Id { id: "item".to_string(), span: Span::new(1, 1) },
                                        ),
                                        span: Span::new(1, 1),
                                    },
                                    ast::Expr {
                                        kind: ast::ExprKind::VarDecl(
                                            ast::VarDecl {
                                                id: ast::Id { id: "local".to_string(), span: Span::new(2, 1) },
                                                r#type: None,
                                            },
                                        ),
                                        span: Span::new(2, 1),
                                    },
                                    ast::Expr {
                                        kind: ast::ExprKind::Id(
                                            ast::Id { id: "local".to_string(), span: Span::new(3, 1) },
                                        ),
                                        span: Span::new(3, 1),
                                    },
                                ],
                            },
                        },
                    ),
                },
            ],
        },
    ];
    let lowering = HirLowering::new(&ast);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir {
            items: hashmap! {
                "myhako::item".into() => (
                    Item::FnDecl(
                        FnDecl {
                            args: Vec::new(),
                            body: Body {
                                locals: vec![
                                    Local::VarDecl(
                                        VarDecl {
                                            mutable: false,
                                        },
                                    ),
                                ],
                                exprs: vec![
                                    Expr::PathRef("myhako::item".into()),
                                    Expr::LocalDecl(0.into()),
                                    Expr::LocalRef(0.into()),
                                ],
                            },
                        },
                    )
                ),
            },
        },
    );
    assert!(logs.is_empty());
}
