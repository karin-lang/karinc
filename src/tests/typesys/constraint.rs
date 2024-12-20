use std::collections::HashMap;

use maplit::hashmap;

use crate::lexer::token::{self, Span};
use crate::parser::ast::{self, tltype::TopLevelTypeTable};
use crate::hir::{self, id::*};
use crate::typesys::{*, resolve::*, table::*};
use crate::typesys::log::TypeLog;

#[test]
fn constrains_types() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type::new(
                                            hir::TypeKind::Prim(ast::PrimType::Bool),
                                        ),
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: vec![
                                    hir::VarDef {
                                        id: ast::Id { id: "id".to_string(), span: Span::new(0, 1) },
                                        r#type: None,
                                        ref_mut: ast::RefMut::None,
                                        init: Some(
                                            hir::Expr {
                                                id: ExprId::new(1),
                                                kind: hir::ExprKind::LocalRef(LocalId::FormalArg(FormalArgId::new(0))),
                                            },
                                        ),
                                    },
                                ],
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::VarDef(VarId::new(0)),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Expr(BodyId::new(0), ExprId::new(1))],
                    None,
                ),
                TypeId::Var(BodyId::new(0), VarId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    Some(TypeId::Expr(BodyId::new(0), ExprId::new(1))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Var(BodyId::new(0), VarId::new(0))],
                    Some(TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn validates_main_fn_signature() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::main".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: Some(
                                    hir::Type {
                                        kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                    },
                                ),
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type::new(
                                            hir::TypeKind::Prim(ast::PrimType::Bool),
                                        ),
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: Vec::new(),
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, Some(&"my_hako::main".into()));

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        hashmap! {
            ModId::new(0, 0) => vec![
                TypeLog::ExpectedMainFnArgsToBeZeroLen { span: Span::new(0, 0) },
                TypeLog::ExpectedMainFnRetTypeToBeNone { span: Span::new(0, 0) },
            ],
        },
    );
}

#[test]
fn constrains_different_body_types() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item1".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type::new(
                                            hir::TypeKind::Prim(ast::PrimType::Bool),
                                        ),
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: Vec::new(),
                            },
                        },
                    ),
                }
            ),
            "my_hako::item2".into() => (
                hir::Item {
                    id: ItemId::new(0, 1),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(1),
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type::new(
                                            hir::TypeKind::Prim(ast::PrimType::Bool),
                                        ),
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: Vec::new(),
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
                TypeId::FormalArg(BodyId::new(1), FormalArgId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_block_type() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    /* constrain with last expression type */
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::Block(
                                            hir::Block {
                                                exprs: vec![
                                                    hir::Expr {
                                                        id: ExprId::new(1),
                                                        kind: hir::ExprKind::Literal(
                                                            token::Literal::Bool { value: true },
                                                        ),
                                                    },
                                                    hir::Expr {
                                                        id: ExprId::new(2),
                                                        kind: hir::ExprKind::Literal(
                                                            token::Literal::Char { value: None },
                                                        ),
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                    /* constrain with none type */
                                    hir::Expr {
                                        id: ExprId::new(3),
                                        kind: hir::ExprKind::Block(
                                            hir::Block {
                                                exprs: Vec::new(),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    Vec::new(),
                    Some(TypeId::Expr(BodyId::new(0), ExprId::new(2))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    vec![TypeId::Expr(BodyId::new(0), ExprId::new(0))],
                    None,
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(3)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_literal_types() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    /* bool literal */
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Bool { value: true },
                                        ),
                                    },
                                    /* int literal */
                                    hir::Expr {
                                        id: ExprId::new(1),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Int {
                                                base: token::Base::Dec,
                                                int_digits: "0".to_string(),
                                                r#type: None,
                                            },
                                        ),
                                    },
                                    /* int literal with type suffix */
                                    hir::Expr {
                                        id: ExprId::new(2),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Int {
                                                base: token::Base::Dec,
                                                int_digits: "0".to_string(),
                                                r#type: Some(ast::PrimType::Usize),
                                            },
                                        ),
                                    },
                                    /* float literal */
                                    hir::Expr {
                                        id: ExprId::new(3),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Float {
                                                digits: Some(
                                                    token::FloatDigits {
                                                        int: "0".to_string(),
                                                        fraction: "0".to_string(),
                                                    },
                                                ),
                                                r#type: None,
                                            },
                                        ),
                                    },
                                    /* float literal with type suffix */
                                    hir::Expr {
                                        id: ExprId::new(4),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Float {
                                                digits: Some(
                                                    token::FloatDigits {
                                                        int: "0".to_string(),
                                                        fraction: "0".to_string(),
                                                    },
                                                ),
                                                r#type: Some(ast::PrimType::F32),
                                            },
                                        ),
                                    },
                                    /* char literal */
                                    hir::Expr {
                                        id: ExprId::new(5),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Char { value: Some('\0') },
                                        ),
                                    },
                                    /* str literal */
                                    hir::Expr {
                                        id: ExprId::new(6),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Str { value: String::new() },
                                        ),
                                    },
                                    /* byte char literal */
                                    hir::Expr {
                                        id: ExprId::new(7),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::ByteChar { value: Some('\0') },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Ambiguous(AmbiguousType::Int)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Usize)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(3)) => TypeConstraint::new(
                    TypePtr::new(Type::Ambiguous(AmbiguousType::Float)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(4)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::F32)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(5)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(6)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Str)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(7)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::U32)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_by_top_level_ref() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: Some(
                                    hir::Type {
                                        kind: Box::new(hir::TypeKind::Prim(ast::PrimType::U8)),
                                    },
                                ),
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::U16)),
                                        },
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::TopLevelRef(TopLevelId::Item(ItemId::new(0, 0)), "my_hako::item".into()),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::U8)),
                arg_types: vec![Type::Prim(ast::PrimType::U16)],
            },
        ),
    }.into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::U16)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(
                        Type::Fn(
                            FnType {
                                ret_type: Box::new(Type::Prim(ast::PrimType::U8)),
                                arg_types: vec![Type::Prim(ast::PrimType::U16)],
                            },
                        ),
                    ),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::Item(ItemId::new(0, 0)))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_by_local_ref() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::U8)),
                                        },
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: vec![
                                    hir::VarDef {
                                        id: ast::Id { id: "id".to_string(), span: Span::new(0, 1) },
                                        r#type: Some(
                                            hir::Type {
                                                kind: Box::new(hir::TypeKind::Prim(ast::PrimType::U16)),
                                            },
                                        ),
                                        ref_mut: ast::RefMut::None,
                                        init: None,
                                    },
                                ],
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::VarDef(VarId::new(0)),
                                    },
                                    hir::Expr {
                                        id: ExprId::new(1),
                                        kind: hir::ExprKind::LocalRef(LocalId::FormalArg(FormalArgId::new(0))),
                                    },
                                    hir::Expr {
                                        id: ExprId::new(2),
                                        kind: hir::ExprKind::LocalRef(LocalId::Var(VarId::new(0))),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U8)),
                    vec![TypeId::Expr(BodyId::new(0), ExprId::new(1))],
                    None,
                ),
                TypeId::Var(BodyId::new(0), VarId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U16)),
                    vec![TypeId::Expr(BodyId::new(0), ExprId::new(2))],
                    None,
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U8)),
                    Vec::new(),
                    Some(TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U16)),
                    Vec::new(),
                    Some(TypeId::Var(BodyId::new(0), VarId::new(0))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn detects_inconsistent_constraint_of_var_init() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: vec![
                                    hir::VarDef {
                                        id: ast::Id { id: "id".to_string(), span: Span::new(0, 1) },
                                        r#type: Some(
                                            hir::Type {
                                                kind: Box::new(hir::TypeKind::Prim(ast::PrimType::I32)),
                                            },
                                        ),
                                        ref_mut: ast::RefMut::None,
                                        init: Some(
                                            hir::Expr {
                                                id: ExprId::new(1),
                                                kind: hir::ExprKind::Literal(
                                                    token::Literal::Bool { value: true },
                                                ),
                                            },
                                        ),
                                    },
                                ],
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::VarDef(VarId::new(0)),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Var(BodyId::new(0), VarId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::I32)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        hashmap! {
            ModId::new(0, 0) => vec![TypeLog::InconsistentConstraint],
        },
    );
}

#[test]
fn constrains_by_ret() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: Some(
                                    hir::Type {
                                        kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                    },
                                ),
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::Ret(
                                            hir::Ret {
                                                value: Box::new(
                                                    hir::Expr {
                                                        id: ExprId::new(1),
                                                        kind: hir::ExprKind::Literal(
                                                            token::Literal::Bool { value: true },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::Bool)),
                arg_types: Vec::new(),
            },
        ),
        TopLevelId::FnRet(ItemId::new(0, 0)) => Type::Prim(ast::PrimType::Bool),
    }.into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnRet(ItemId::new(0, 0)))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_by_unmatch_type_ret() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::Ret(
                                            hir::Ret {
                                                value: Box::new(
                                                    hir::Expr {
                                                        id: ExprId::new(1),
                                                        kind: hir::ExprKind::Literal(
                                                            token::Literal::Bool { value: true },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::None)),
                arg_types: Vec::new(),
            },
        ),
        TopLevelId::FnRet(ItemId::new(0, 0)) => Type::Prim(ast::PrimType::None),
    }.into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        hashmap! {
            ModId::new(0, 0) => vec![TypeLog::InconsistentConstraint],
        },
    );
}

#[test]
fn constrains_by_fn_call() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                        },
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::FnCall(
                                            hir::FnCall {
                                                r#fn: Some((ItemId::new(0, 0), "my_hako::item".into())),
                                                args: vec![
                                                    hir::ActualArg {
                                                        expr: hir::Expr {
                                                            id: ExprId::new(1),
                                                            kind: hir::ExprKind::Literal(
                                                                token::Literal::Bool { value: true },
                                                            ),
                                                        },
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::None)),
                arg_types: vec![Type::Prim(ast::PrimType::Bool)],
            },
        ),
        TopLevelId::FnRet(ItemId::new(0, 0)) => Type::Prim(ast::PrimType::None),
        TopLevelId::FnArg(ItemId::new(0, 0), FormalArgId::new(0)) => Type::Prim(ast::PrimType::Bool),
    }.into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    None,
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnRet(ItemId::new(0, 0)))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnArg(ItemId::new(0, 0), FormalArgId::new(0)))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_fn_call_with_no_item_id_as_unresolved() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::FnCall(
                                            hir::FnCall {
                                                r#fn: None,
                                                args: Vec::new(),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Unknown),
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        hashmap! {
            ModId::new(0, 0) => vec![
                TypeLog::UnknownType { type_id: TypeId::Expr(BodyId::new(0), ExprId::new(0)) },
            ],
        },
    );
}

#[test]
fn detects_inconsistent_constraint_of_fn_call() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                        },
                                        ref_mut: ast::RefMut::None,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::FnCall(
                                            hir::FnCall {
                                                r#fn: Some((ItemId::new(0, 0), "my_hako::item".into())),
                                                args: Vec::new(),
                                            },
                                        ),
                                    },
                                    hir::Expr {
                                        id: ExprId::new(1),
                                        kind: hir::ExprKind::FnCall(
                                            hir::FnCall {
                                                r#fn: Some((ItemId::new(0, 0), "my_hako::item".into())),
                                                args: vec![
                                                    hir::ActualArg {
                                                        expr: hir::Expr {
                                                            id: ExprId::new(2),
                                                            kind: hir::ExprKind::Literal(
                                                                token::Literal::Char { value: Some('\0') },
                                                            ),
                                                        },
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::None)),
                arg_types: vec![Type::Prim(ast::PrimType::Bool)],
            },
        ),
        TopLevelId::FnRet(ItemId::new(0, 0)) => Type::Prim(ast::PrimType::None),
        TopLevelId::FnArg(ItemId::new(0, 0), FormalArgId::new(0)) => Type::Prim(ast::PrimType::Bool),
    }.into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::FormalArg(BodyId::new(0), FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    None,
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnRet(ItemId::new(0, 0)))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnRet(ItemId::new(0, 0)))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    Vec::new(),
                    None,
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        hashmap! {
            ModId::new(0, 0) => vec![
                TypeLog::FnCallWithInvalidArgLen { expected: 1, provided: 0 },
                TypeLog::InconsistentConstraint,
            ],
        },
    );
}

#[test]
fn constrains_var_by_bind() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: vec![
                                    hir::VarDef {
                                        id: ast::Id { id: "id".to_string(), span: Span::new(0, 1) },
                                        r#type: None,
                                        ref_mut: ast::RefMut::None,
                                        init: None,
                                    },
                                ],
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::VarDef(VarId::new(0)),
                                    },
                                    hir::Expr {
                                        id: ExprId::new(1),
                                        kind: hir::ExprKind::VarBind(
                                            hir::VarBind {
                                                var_id: VarId::new(0),
                                                value: Box::new(
                                                    hir::Expr {
                                                        id: ExprId::new(2),
                                                        kind: hir::ExprKind::Literal(
                                                            token::Literal::Bool { value: true },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Var(BodyId::new(0), VarId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Expr(BodyId::new(0), ExprId::new(2))],
                    None,
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    Some(TypeId::Var(BodyId::new(0), VarId::new(0))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn detects_inconsistent_constraint_of_var_bind() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: vec![
                                    hir::VarDef {
                                            id: ast::Id { id: "id".to_string(), span: Span::new(0, 1) },
                                            r#type: Some(
                                            hir::Type::new(
                                                hir::TypeKind::Prim(ast::PrimType::Usize),
                                            ),
                                        ),
                                        ref_mut: ast::RefMut::None,
                                        init: None,
                                    },
                                ],
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::VarDef(VarId::new(0)),
                                    },
                                    hir::Expr {
                                        id: ExprId::new(1),
                                        kind: hir::ExprKind::VarBind(
                                            hir::VarBind {
                                                var_id: VarId::new(0),
                                                value: Box::new(
                                                    hir::Expr {
                                                        id: ExprId::new(2),
                                                        kind: hir::ExprKind::Literal(
                                                            token::Literal::Bool { value: true },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);
    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Var(BodyId::new(0), VarId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Usize)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        hashmap! {
            ModId::new(0, 0) => vec![TypeLog::InconsistentConstraint],
        },
    );
}

#[test]
fn detects_invalid_types_on_finalization() {
    let type_table = hashmap! {
        TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
            TypePtr::new(Type::Unknown),
        ),
        TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
            TypePtr::new(Type::Unknown),
        ),
    }.into();
    let top_level_type_table = TopLevelTypeTable::new();
    let builder = TypeTableBuilder::from_table(&top_level_type_table, type_table);
    let (_, logs) = builder.finalize();

    assert_eq!(
        logs,
        hashmap! {
            ModId::new(0, 0) => vec![
                TypeLog::UnknownType { type_id: TypeId::Expr(BodyId::new(0), ExprId::new(0)) },
                TypeLog::UnknownType { type_id: TypeId::Expr(BodyId::new(0), ExprId::new(1)) },
            ],
        },
    );
}

#[test]
fn constrains_if_type() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::If(
                                            hir::If {
                                                cond: Box::new(
                                                    hir::Expr {
                                                        id: ExprId::new(1),
                                                        kind: hir::ExprKind::Literal(token::Literal::Bool { value: true }),
                                                    },
                                                ),
                                                block: hir::Block {
                                                    exprs: vec![
                                                        hir::Expr {
                                                            id: ExprId::new(2),
                                                            kind: hir::ExprKind::Literal(token::Literal::Char { value: None }),
                                                        },
                                                    ],
                                                },
                                                elifs: vec![
                                                    hir::Elif {
                                                        cond: Box::new(
                                                            hir::Expr {
                                                                id: ExprId::new(3),
                                                                kind: hir::ExprKind::Literal(token::Literal::Bool { value: true }),
                                                            },
                                                        ),
                                                        block: hir::Block {
                                                            exprs: vec![
                                                                hir::Expr {
                                                                    id: ExprId::new(4),
                                                                    kind: hir::ExprKind::Literal(token::Literal::Char { value: None }),
                                                                },
                                                            ],
                                                        },
                                                    },
                                                ],
                                                r#else: Some(
                                                    hir::Block {
                                                        exprs: vec![
                                                            hir::Expr {
                                                                id: ExprId::new(5),
                                                                kind: hir::ExprKind::Literal(token::Literal::Char { value: None }),
                                                            },
                                                        ],
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    vec![
                        TypeId::Expr(BodyId::new(0), ExprId::new(2)),
                        TypeId::Expr(BodyId::new(0), ExprId::new(4)),
                        TypeId::Expr(BodyId::new(0), ExprId::new(5)),
                    ],
                    None,
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    Vec::new(),
                    Some(TypeId::Expr(BodyId::new(0), ExprId::new(0))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(3)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(4)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    Vec::new(),
                    Some(TypeId::Expr(BodyId::new(0), ExprId::new(0))),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(5)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    Vec::new(),
                    Some(TypeId::Expr(BodyId::new(0), ExprId::new(0))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_if_type_with_none() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::If(
                                            hir::If {
                                                cond: Box::new(
                                                    hir::Expr {
                                                        id: ExprId::new(1),
                                                        kind: hir::ExprKind::Literal(token::Literal::Bool { value: true }),
                                                    },
                                                ),
                                                block: hir::Block {
                                                    exprs: Vec::new(),
                                                },
                                                elifs: vec![
                                                    hir::Elif {
                                                        cond: Box::new(
                                                            hir::Expr {
                                                                id: ExprId::new(2),
                                                                kind: hir::ExprKind::Literal(token::Literal::Bool { value: true }),
                                                            },
                                                        ),
                                                        block: hir::Block {
                                                            exprs: Vec::new(),
                                                        },
                                                    },
                                                ],
                                                r#else: Some(
                                                    hir::Block {
                                                        exprs: Vec::new(),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::None)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(2)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrains_endless_for_type() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    mod_id: ModId::new(0, 0),
                    marker: hir::MarkerInfo::new(),
                    accessibility: ast::Accessibility::Default,
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                id: BodyId::new(0),
                                ret_type: None,
                                args: Vec::new(),
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::For(
                                            hir::For {
                                                kind: hir::ForKind::Endless,
                                                block: hir::Block {
                                                    exprs: vec![
                                                        hir::Expr {
                                                            id: ExprId::new(1),
                                                            kind: hir::ExprKind::Literal(
                                                                token::Literal::Bool { value: true },
                                                            ),
                                                        },
                                                    ],
                                                },
                                            },
                                        ),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
        todos: Vec::new(),
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeResolver::resolve(&hir, &top_level_type_table, None);

    assert_eq!(
        table.to_sorted_vec(),
        TypeTable::from(
            hashmap! {
                TypeId::Expr(BodyId::new(0), ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Expr(BodyId::new(0), ExprId::new(1))],
                    None,
                ),
                TypeId::Expr(BodyId::new(0), ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    Some(TypeId::Expr(BodyId::new(0), ExprId::new(0))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(logs.is_empty());
}
