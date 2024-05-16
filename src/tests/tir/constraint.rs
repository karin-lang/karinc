use std::collections::HashMap;

use maplit::hashmap;

use crate::lexer::token;
use crate::parser::ast::{self, tltype::TopLevelTypeTable};
use crate::hir::{self, id::*};
use crate::tir::r#type::*;
use crate::tir::constraint::{*, lower::*};

#[test]
fn constrains_types() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type::new(
                                            hir::TypeKind::Prim(ast::PrimType::Bool),
                                        ),
                                        mutable: false,
                                    },
                                ],
                                vars: vec![
                                    hir::VarDef {
                                        r#type: None,
                                        mutable: false,
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
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::FormalArg(FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Expr(ExprId::new(1))],
                    None,
                ),
                TypeId::Var(VarId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    Some(TypeId::Expr(ExprId::new(1))),
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Var(VarId::new(0))],
                    Some(TypeId::FormalArg(FormalArgId::new(0))),
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
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
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
                                                base: token::Base::Dec,
                                                int_digits: "0".to_string(),
                                                fraction_digits: "0".to_string(),
                                                r#type: None,
                                            },
                                        ),
                                    },
                                    /* float literal with type suffix */
                                    hir::Expr {
                                        id: ExprId::new(4),
                                        kind: hir::ExprKind::Literal(
                                            token::Literal::Float {
                                                base: token::Base::Dec,
                                                int_digits: "0".to_string(),
                                                fraction_digits: "0".to_string(),
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
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Infer(InferType::Int)),
                ),
                TypeId::Expr(ExprId::new(2)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Usize)),
                ),
                TypeId::Expr(ExprId::new(3)) => TypeConstraint::new(
                    TypePtr::new(Type::Infer(InferType::Float)),
                ),
                TypeId::Expr(ExprId::new(4)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::F32)),
                ),
                TypeId::Expr(ExprId::new(5)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                ),
                TypeId::Expr(ExprId::new(6)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Str)),
                ),
                TypeId::Expr(ExprId::new(7)) => TypeConstraint::new(
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
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
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
                                        mutable: false,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::TopLevelRef(TopLevelId::Item(ItemId::new(0, 0))),
                                    },
                                ],
                            },
                        },
                    ),
                }
            ),
        },
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::U8)),
                arg_types: vec![Type::Prim(ast::PrimType::U16)],
            },
        ),
    }.into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::FormalArg(FormalArgId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::U16)),
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new_constrained(
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
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::U8)),
                                        },
                                        mutable: false,
                                    },
                                ],
                                vars: vec![
                                    hir::VarDef {
                                        r#type: Some(
                                            hir::Type {
                                                kind: Box::new(hir::TypeKind::Prim(ast::PrimType::U16)),
                                            },
                                        ),
                                        mutable: false,
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
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::FormalArg(FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U8)),
                    vec![TypeId::Expr(ExprId::new(1))],
                    None,
                ),
                TypeId::Var(VarId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U16)),
                    vec![TypeId::Expr(ExprId::new(2))],
                    None,
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U8)),
                    Vec::new(),
                    Some(TypeId::FormalArg(FormalArgId::new(0))),
                ),
                TypeId::Expr(ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::U16)),
                    Vec::new(),
                    Some(TypeId::Var(VarId::new(0))),
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
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                ret_type: None,
                                args: Vec::new(),
                                vars: vec![
                                    hir::VarDef {
                                        r#type: Some(
                                            hir::Type {
                                                kind: Box::new(hir::TypeKind::Prim(ast::PrimType::I32)),
                                            },
                                        ),
                                        mutable: false,
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
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::Var(VarId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::I32)),
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Var(VarId::new(0))],
                    None,
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        vec![
            TypeLog::InconsistentConstraint,
        ],
    );
}

#[test]
fn constrains_by_fn_call() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                        },
                                        mutable: false,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::FnCall(
                                            hir::FnCall {
                                                r#fn: ItemId::new(0, 0),
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
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::Void)),
                arg_types: vec![Type::Prim(ast::PrimType::Bool)],
            },
        ),
        TopLevelId::FnRet(ItemId::new(0, 0)) => Type::Prim(ast::PrimType::Void),
        TopLevelId::FnArg(ItemId::new(0, 0), FormalArgId::new(0)) => Type::Prim(ast::PrimType::Bool),
    }.into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::FormalArg(FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    None,
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnRet(ItemId::new(0, 0)))),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new_constrained(
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
fn detects_inconsistent_constraint_of_fn_call() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                ret_type: None,
                                args: vec![
                                    hir::FormalArgDef {
                                        id: FormalArgId::new(0),
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                        },
                                        mutable: false,
                                    },
                                ],
                                vars: Vec::new(),
                                exprs: vec![
                                    hir::Expr {
                                        id: ExprId::new(0),
                                        kind: hir::ExprKind::FnCall(
                                            hir::FnCall {
                                                r#fn: ItemId::new(0, 0),
                                                args: Vec::new(),
                                            },
                                        ),
                                    },
                                    hir::Expr {
                                        id: ExprId::new(1),
                                        kind: hir::ExprKind::FnCall(
                                            hir::FnCall {
                                                r#fn: ItemId::new(0, 0),
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
    };
    let top_level_type_table = hashmap! {
        TopLevelId::Item(ItemId::new(0, 0)) => Type::Fn(
            FnType {
                ret_type: Box::new(Type::Prim(ast::PrimType::Void)),
                arg_types: vec![Type::Prim(ast::PrimType::Bool)],
            },
        ),
        TopLevelId::FnRet(ItemId::new(0, 0)) => Type::Prim(ast::PrimType::Void),
        TopLevelId::FnArg(ItemId::new(0, 0), FormalArgId::new(0)) => Type::Prim(ast::PrimType::Bool),
    }.into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::FormalArg(FormalArgId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    None,
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnRet(ItemId::new(0, 0)))),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::FnRet(ItemId::new(0, 0)))),
                ),
                TypeId::Expr(ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Char)),
                    Vec::new(),
                    None,
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(
        logs,
        vec![
            TypeLog::FnCallWithInvalidArgLen { expected: 1, provided: 0 },
            TypeLog::InconsistentConstraint,
        ],
    );
}

#[test]
fn constrains_var_by_bind() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item {
                    id: ItemId::new(0, 0),
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                ret_type: None,
                                args: Vec::new(),
                                vars: vec![
                                    hir::VarDef {
                                        r#type: None,
                                        mutable: false,
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
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);

    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::Var(VarId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    vec![TypeId::Expr(ExprId::new(2))],
                    None,
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                ),
                TypeId::Expr(ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    Some(TypeId::Var(VarId::new(0))),
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
                    kind: hir::ItemKind::FnDecl(
                        hir::FnDecl {
                            body: hir::Body {
                                ret_type: None,
                                args: Vec::new(),
                                vars: vec![
                                    hir::VarDef {
                                        r#type: Some(
                                            hir::Type::new(
                                                hir::TypeKind::Prim(ast::PrimType::Usize),
                                            ),
                                        ),
                                        mutable: false,
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
    };
    let top_level_type_table = HashMap::new().into();
    let (table, logs) = TypeConstraintLowering::lower(&hir, &top_level_type_table);
    assert_eq!(
        table.to_sorted_vec(),
        TypeConstraintTable::from(
            hashmap! {
                TypeId::Var(VarId::new(0)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Usize)),
                    Vec::new(),
                    None,
                ),
                TypeId::Expr(ExprId::new(0)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Prim(ast::PrimType::Void)),
                ),
                TypeId::Expr(ExprId::new(2)) => TypeConstraint::new_constrained(
                    TypePtr::new(Type::Prim(ast::PrimType::Bool)),
                    Vec::new(),
                    None,
                ),
            },
        ).to_sorted_vec(),
    );
    assert_eq!(logs, vec![TypeLog::InconsistentConstraint]);
}

#[test]
fn detects_invalid_types_on_finalization() {
    let type_constraint_table = hashmap! {
        TypeId::Expr(ExprId::new(0)) => TypeConstraint::new(
            TypePtr::new(Type::Undefined),
        ),
        TypeId::Expr(ExprId::new(1)) => TypeConstraint::new(
            TypePtr::new(Type::Unresolved),
        ),
    }.into();
    let top_level_type_table = TopLevelTypeTable::new();
    let builder = TypeConstraintBuilder::from_table(&top_level_type_table, type_constraint_table);

    assert_eq!(
        builder.finalize(),
        vec![
            TypeLog::UndefinedType { type_id: TypeId::Expr(ExprId::new(0)) },
            TypeLog::UnresolvedType { type_id: TypeId::Expr(ExprId::new(1)) },
        ],
    );
}
