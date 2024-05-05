use maplit::hashmap;

use crate::lexer::token;
use crate::tir::constraint::*;
use crate::hir::{self, id::*};
use crate::parser::ast;

#[test]
fn constrains_types() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item::FnDecl(
                    hir::FnDecl {
                        body: hir::Body {
                            args: vec![
                                hir::FormalArgDef {
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
                )
            ),
        },
    };
    let (table, logs) = TypeConstraintBuilder::build(&hir);

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
                    TypePtr::new(Type::Void),
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
fn constrains_var_by_bind() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item::FnDecl(
                    hir::FnDecl {
                        body: hir::Body {
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
                )
            ),
        },
    };
    let (table, logs) = TypeConstraintBuilder::build(&hir);

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
                    TypePtr::new(Type::Void),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Void),
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
                hir::Item::FnDecl(
                    hir::FnDecl {
                        body: hir::Body {
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
                )
            ),
        },
    };
    let (table, logs) = TypeConstraintBuilder::build(&hir);

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
                    TypePtr::new(Type::Void),
                ),
                TypeId::Expr(ExprId::new(1)) => TypeConstraint::new(
                    TypePtr::new(Type::Void),
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
