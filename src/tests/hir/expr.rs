use std::collections::HashMap;

use log::HirLoweringLog;
use maplit::hashmap;

use crate::lexer::token::{self, Span};
use crate::parser::ast;
use crate::hir::*;
use crate::hir::id::*;
use crate::hir::lower::HirLowering;

#[test]
fn lowers_block_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::Block(
            ast::Block {
                exprs: vec![
                    ast::Expr {
                        kind: ast::ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                        span: Span::new(0, 1),
                    },
                ],
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
            kind: ExprKind::Block(
                Block {
                    exprs: vec![
                        Expr {
                            id: ExprId::new(1),
                            kind: ExprKind::Literal(
                                token::Literal::Bool { value: true },
                            ),
                        },
                    ],
                },
            ),
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn lowers_top_level_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::Path("seg1::seg2".into()),
        span: Span::new(0, 1),
    };
    let asts = Vec::new();
    let paths = hashmap! {
        "seg1::seg2".into() => GlobalId::Item(ItemId::new(0, 0)),
    };
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_expr(&ast);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::TopLevelRef(TopLevelId::Item(ItemId::new(0, 0)), "seg1::seg2".into()),
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn lowers_unnecessary_top_level_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::Path("seg1::seg2".into()),
        span: Span::new(0, 1),
    };
    let asts = Vec::new();
    let paths = hashmap! {
        "seg1::seg2".into() => GlobalId::Hako(HakoId::new(0)),
    };
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_expr(&ast);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::Unknown,
        },
    );
    assert_eq!(
        *lowering.get_logs(),
        hashmap! {
            ModId::new(0, 0) => vec![
                HirLoweringLog::UnnecessaryPath { path: "seg1::seg2".into(), span: Span::new(0, 1) }
            ],
        },
    );
}

#[test]
fn lowers_literal_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::Literal(
            token::Literal::Bool { value: true },
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
            kind: ExprKind::Literal(
                token::Literal::Bool { value: true },
            ),
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn lowers_ret_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::Ret(
            ast::Ret {
                value: Box::new(
                    ast::Expr {
                        kind: ast::ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                        span: Span::new(1, 1),
                    },
                ),
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
            kind: ExprKind::Ret(
                Ret {
                    value: Box::new(
                        Expr {
                            id: ExprId::new(1),
                            kind: ExprKind::Literal(
                                token::Literal::Bool { value: true },
                            ),
                        },
                    ),
                },
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
                    r#fn: Some(ItemId::new(0, 0)),
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
                id: ast::Id { id: "i".to_string(), span: Span::new(0, 1) },
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

#[test]
fn lowers_var_bind_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::VarBind(
            ast::VarBind {
                id: ast::Id { id: "id".to_string(), span: Span::new(0, 1) },
                value: Box::new(
                    ast::Expr {
                        kind: ast::ExprKind::Literal(
                            token::Literal::Bool { value: true },
                        ),
                        span: Span::new(1, 1),
                    },
                ),
            },
        ),
        span: Span::new(0, 1),
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    lowering.get_body_scope_hierarchy_mut().declare(
        "id",
        LocalDef::Var(
            VarDef {
                id: ast::Id { id: "id".to_string(), span: Span::new(1, 1) },
                ref_mut: ast::RefMut::None,
                r#type: None,
                init: None,
            },
        ),
    );
    let hir = lowering.lower_expr(&ast);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::VarBind(
                VarBind {
                    var_id: VarId::new(0),
                    value: Box::new(
                        Expr {
                            id: ExprId::new(1),
                            kind: ExprKind::Literal(
                                token::Literal::Bool { value: true },
                            ),
                        },
                    ),
                },
            ),
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn lowers_if_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::If(
            ast::If {
                cond: Box::new(
                    ast::Expr {
                        kind: ast::ExprKind::Id(
                            ast::Id { id: "cond".to_string(), span: Span::new(1, 1) }
                        ),
                        span: Span::new(1, 1),
                    },
                ),
                block: ast::Block {
                    exprs: Vec::new(),
                },
                elifs: vec![
                    ast::Elif {
                        cond: Box::new(
                            ast::Expr {
                                kind: ast::ExprKind::Id(
                                    ast::Id { id: "cond".to_string(), span: Span::new(5, 1) }
                                ),
                                span: Span::new(5, 1),
                            },
                        ),
                        block: ast::Block {
                            exprs: Vec::new(),
                        },
                    },
                    ast::Elif {
                        cond: Box::new(
                            ast::Expr {
                                kind: ast::ExprKind::Id(
                                    ast::Id { id: "cond".to_string(), span: Span::new(9, 1) }
                                ),
                                span: Span::new(9, 1),
                            },
                        ),
                        block: ast::Block {
                            exprs: Vec::new(),
                        },
                    },
                ],
                r#else: Some(
                    ast::Block {
                        exprs: Vec::new(),
                    },
                ),
            },
        ),
        span: Span::new(0, 1),
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    lowering.get_body_scope_hierarchy_mut().declare(
        "cond",
        LocalDef::Var(
            VarDef {
                id: ast::Id { id: "cond".to_string(), span: Span::new(0, 1) },
                ref_mut: ast::RefMut::None,
                r#type: None,
                init: None,
            },
        ),
    );
    let hir = lowering.lower_expr(&ast);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::If(
                If {
                    cond: Box::new(
                        Expr {
                            id: ExprId::new(1),
                            kind: ExprKind::LocalRef(LocalId::Var(VarId::new(0))),
                        },
                    ),
                    block: Block {
                        exprs: Vec::new(),
                    },
                    elifs: vec![
                        Elif {
                            cond: Box::new(
                                Expr {
                                    id: ExprId::new(2),
                                    kind: ExprKind::LocalRef(LocalId::Var(VarId::new(0))),
                                },
                            ),
                            block: Block {
                                exprs: Vec::new(),
                            },
                        },
                        Elif {
                            cond: Box::new(
                                Expr {
                                    id: ExprId::new(3),
                                    kind: ExprKind::LocalRef(LocalId::Var(VarId::new(0))),
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
            ),
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn lowers_for_expr() {
    let ast = ast::Expr {
        kind: ast::ExprKind::For(
            ast::For {
                kind: ast::ForKind::Range {
                    index: Box::new(
                        ast::Expr {
                            kind: ast::ExprKind::Id(
                                ast::Id { id: "index".to_string(), span: Span::new(1, 1) }
                            ),
                            span: Span::new(1, 1),
                        },
                    ),
                    range: Box::new(
                        ast::Expr {
                            kind: ast::ExprKind::Id(
                                ast::Id { id: "range".to_string(), span: Span::new(3, 1) }
                            ),
                            span: Span::new(3, 1),
                        },
                    ),
                },
                block: ast::Block {
                    exprs: Vec::new(),
                },
            },
        ),
        span: Span::new(0, 1),
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    lowering.get_body_scope_hierarchy_mut().declare(
        "index",
        LocalDef::Var(
            VarDef {
                id: ast::Id { id: "index".to_string(), span: Span::new(0, 1) },
                ref_mut: ast::RefMut::None,
                r#type: None,
                init: None,
            },
        ),
    );
    lowering.get_body_scope_hierarchy_mut().declare(
        "range",
        LocalDef::Var(
            VarDef {
                id: ast::Id { id: "range".to_string(), span: Span::new(1, 1) },
                ref_mut: ast::RefMut::None,
                r#type: None,
                init: None,
            },
        ),
    );
    let hir = lowering.lower_expr(&ast);

    assert_eq!(
        hir,
        Expr {
            id: ExprId::new(0),
            kind: ExprKind::For(
                For {
                    kind: ForKind::Range {
                        index: Box::new(
                            Expr {
                                id: ExprId::new(1),
                                kind: ExprKind::LocalRef(LocalId::Var(VarId::new(0))),
                            },
                        ),
                        range: Box::new(
                            Expr {
                                id: ExprId::new(2),
                                kind: ExprKind::LocalRef(LocalId::Var(VarId::new(1))),
                            },
                        ),
                    },
                    block: Block {
                        exprs: Vec::new(),
                    },
                },
            ),
        },
    );
    assert!(lowering.get_logs().is_empty());
}
