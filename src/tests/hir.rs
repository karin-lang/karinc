pub mod expr;
pub mod item;

use std::collections::HashMap;

use maplit::hashmap;

use crate::lexer::token::Span;
use crate::parser::ast;
use crate::hir::*;
use crate::hir::id::*;
use crate::hir::lower::HirLowering;

#[test]
fn lowers_empty_ast() {
    let ast = ast::Ast {
        mod_id: ModId::new(0, 0),
        mod_path: "my_hako".into(),
        items: Vec::new(),
    };
    let asts = vec![&ast];
    let lowering = HirLowering::new(&asts);
    let (hir, logs) = lowering.lower();

    assert_eq!(hir, Hir { items: HashMap::new() });
    assert!(logs.is_empty());
}

#[test]
fn lowers_subitem_in_mod() {
    let ast = ast::Ast {
        mod_id: ModId::new(0, 0),
        mod_path: "my_hako".into(),
        items: vec![
            ast::Item {
                id: ItemId::new(0, 0),
                name: ast::Id { id: "f".to_string(), span: Span::new(0, 1) },
                accessibility: ast::Accessibility::Default,
                kind: ast::ItemKind::FnDecl(
                    ast::FnDecl {
                        body: ast::Body {
                            id: BodyId::new(0),
                            ret_type: None,
                            args: Vec::new(),
                            exprs: Vec::new(),
                        },
                    },
                ),
            },
        ],
    };
    let asts = vec![&ast];
    let lowering = HirLowering::new(&asts);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir {
            items: hashmap! {
                "my_hako::f".into() => (
                    Item {
                        id: ItemId::new(0, 0),
                        accessibility: ast::Accessibility::Default,
                        kind: ItemKind::FnDecl(
                            FnDecl {
                                body: Body {
                                    id: BodyId::new(0),
                                    ret_type: None,
                                    args: Vec::new(),
                                    vars: Vec::new(),
                                    exprs: Vec::new(),
                                },
                            },
                        ),
                    }
                ),
            },
        },
    );
    assert!(logs.is_empty());
}

#[test]
fn resolves_item_and_local() {
    let ast = ast::Ast {
        mod_id: ModId::new(0, 0),
        mod_path: "my_hako".into(),
        items: vec![
            ast::Item {
                id: ItemId::new(0, 0),
                name: ast::Id { id: "item".to_string(), span: Span::new(0, 1) },
                accessibility: ast::Accessibility::Default,
                kind: ast::ItemKind::FnDecl(
                    ast::FnDecl {
                        body: ast::Body {
                            id: BodyId::new(0),
                            ret_type: None,
                            args: Vec::new(),
                            exprs: vec![
                                ast::Expr {
                                    kind: ast::ExprKind::Id(
                                        ast::Id { id: "item".to_string(), span: Span::new(1, 1) },
                                    ),
                                    span: Span::new(1, 1),
                                },
                                ast::Expr {
                                    kind: ast::ExprKind::VarDef(
                                        ast::VarDef {
                                            id: ast::Id { id: "local".to_string(), span: Span::new(2, 1) },
                                            ref_mut: ast::RefMut::None,
                                            r#type: None,
                                            init: None,
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
    };
    let asts = vec![&ast];
    let lowering = HirLowering::new(&asts);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir {
            items: hashmap! {
                "my_hako::item".into() => (
                    Item {
                        id: ItemId::new(0, 0),
                        accessibility: ast::Accessibility::Default,
                        kind: ItemKind::FnDecl(
                            FnDecl {
                                body: Body {
                                    id: BodyId::new(0),
                                    ret_type: None,
                                    args: Vec::new(),
                                    vars: vec![
                                        VarDef {
                                            id: ast::Id { id: "local".to_string(), span: Span::new(2, 1) },
                                            r#type: None,
                                            ref_mut: ast::RefMut::None,
                                            init: None,
                                        },
                                    ],
                                    exprs: vec![
                                        Expr {
                                            id: ExprId::new(0),
                                            kind: ExprKind::TopLevelRef(
                                                TopLevelId::Item(ItemId::new(0, 0)),
                                            ),
                                        },
                                        Expr {
                                            id: ExprId::new(1),
                                            kind: ExprKind::VarDef(VarId::new(0)),
                                        },
                                        Expr {
                                            id: ExprId::new(2),
                                            kind: ExprKind::LocalRef(LocalId::Var(VarId::new(0))),
                                        },
                                    ],
                                },
                            },
                        ),
                    }
                ),
            },
        },
    );
    assert!(logs.is_empty());
}
