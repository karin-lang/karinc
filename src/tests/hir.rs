use crate::hir::*;
use crate::hir::lower::HirLowering;
use crate::lexer::token::Span;
use crate::parser::ast;

#[test]
fn lowers_empty_ast() {
    let ast = ast::Ast { items: Vec::new(), hako_mods: Vec::new() };
    let lowering = HirLowering::new(&ast);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir { items: Vec::new() },
    );
    assert!(logs.is_empty());
}

#[test]
fn lowers_hako_mod_item() {
    let ast = ast::Ast {
        items: vec![
            ast::Item {
                node_id: 0.into(),
                id: ast::Id { id: "my_hako".to_string(), span: Span::new(0, 0, 1) },
                kind: ast::ItemKind::Mod(
                    ast::Mod {
                        submods: Vec::new(),
                        items: Vec::new(),
                    },
                ),
            },
        ],
        hako_mods: vec![0.into()],
    };
    let lowering = HirLowering::new(&ast);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir { items: vec![Item::Mod] },
    );
    assert!(logs.is_empty());
}

#[test]
fn does_not_lower_mod_item_not_in_hierarchy() {
    let ast = ast::Ast {
        items: vec![
            ast::Item {
                node_id: 0.into(),
                id: ast::Id { id: "my_hako".to_string(), span: Span::new(0, 0, 1) },
                kind: ast::ItemKind::Mod(
                    ast::Mod {
                        submods: Vec::new(),
                        items: Vec::new(),
                    },
                ),
            },
        ],
        hako_mods: Vec::new(),
    };
    let lowering = HirLowering::new(&ast);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir { items: Vec::new() },
    );
    assert!(logs.is_empty());
}

#[test]
fn lowers_subitem_in_mod() {
    let ast = ast::Ast {
        items: vec![
            ast::Item {
                node_id: 0.into(),
                id: ast::Id { id: "my_hako".to_string(), span: Span::new(0, 0, 1) },
                kind: ast::ItemKind::Mod(
                    ast::Mod {
                        submods: Vec::new(),
                        items: vec![("f".to_string(), 1.into())],
                    },
                ),
            },
            ast::Item {
                node_id: 1.into(),
                id: ast::Id { id: "f".to_string(), span: Span::new(0, 1, 1) },
                kind: ast::ItemKind::FnDecl(
                    ast::FnDecl {
                        args: Vec::new(),
                        ret_type: None,
                        body: ast::Body { exprs: Vec::new() },
                    },
                ),
            },
        ],
        hako_mods: vec![0.into()],
    };
    let lowering = HirLowering::new(&ast);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir {
            items: vec![
                Item::Mod,
                Item::FnDecl(
                    FnDecl {
                        args: Vec::new(),
                        body: Body {
                            locals: Vec::new(),
                            exprs: Vec::new(),
                        },
                    },
                ),
            ],
        },
    );
    assert!(logs.is_empty());
}

#[test]
fn resolves_item_and_local() {
    let ast = ast::Ast {
        items: vec![
            ast::Item {
                node_id: 0.into(),
                id: ast::Id { id: "my_hako".to_string(), span: Span::new(0, 0, 1) },
                kind: ast::ItemKind::Mod(
                    ast::Mod {
                        submods: Vec::new(),
                        items: vec![("item".to_string(), 1.into())],
                    },
                ),
            },
            ast::Item {
                node_id: 1.into(),
                id: ast::Id { id: "item".to_string(), span: Span::new(0, 1, 1) },
                kind: ast::ItemKind::FnDecl(
                    ast::FnDecl {
                        args: Vec::new(),
                        ret_type: None,
                        body: ast::Body {
                            exprs: vec![
                                ast::Expr {
                                    kind: ast::ExprKind::Id(
                                        ast::Id { id: "item".to_string(), span: Span::new(0, 2, 1) },
                                    ),
                                    span: Span::new(0, 2, 1),
                                },
                                ast::Expr {
                                    kind: ast::ExprKind::VarDecl(
                                        ast::VarDecl {
                                            id: ast::Id { id: "local".to_string(), span: Span::new(0, 3, 1) },
                                            r#type: None,
                                        },
                                    ),
                                    span: Span::new(0, 3, 1),
                                },
                                ast::Expr {
                                    kind: ast::ExprKind::Id(
                                        ast::Id { id: "local".to_string(), span: Span::new(0, 4, 1) },
                                    ),
                                    span: Span::new(0, 4, 1),
                                },
                            ],
                        },
                    },
                ),
            },
        ],
        hako_mods: vec![0.into()],
    };
    let lowering = HirLowering::new(&ast);
    let (hir, logs) = lowering.lower();

    assert_eq!(
        hir,
        Hir {
            items: vec![
                Item::Mod,
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
                                Expr::ItemRef(1.into()),
                                Expr::LocalDecl(0.into()),
                                Expr::LocalRef(0.into()),
                            ],
                        },
                    },
                ),
            ],
        },
    );
    assert!(logs.is_empty());
}
