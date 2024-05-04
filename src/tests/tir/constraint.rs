use maplit::hashmap;

use crate::lexer::token;
use crate::tir::constraint::*;
use crate::hir::{self, id::*};
use crate::parser::ast;

#[test]
fn constrain_types() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::item".into() => (
                hir::Item::FnDecl(
                    hir::FnDecl {
                        body: hir::Body {
                            args: vec![
                                hir::FormalArgDef {
                                    expr_id: ExprId::new(0),
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
                                            id: ExprId::new(2),
                                            kind: hir::ExprKind::LocalRef(LocalId::FormalArg(FormalArgId::new(0))),
                                        },
                                    ),
                                },
                            ],
                            exprs: vec![
                                hir::Expr {
                                    id: ExprId::new(1),
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
        table,
        hashmap! {
            ExprId::new(0) => TypeConstraint::Independent {
                ptr: TypePtr::new(Type::Prim(ast::PrimType::Bool)),
            },
            ExprId::new(1) => TypeConstraint::Dependent {
                constrained_by: ExprId::new(2),
                ptr: TypePtr::new(Type::Prim(ast::PrimType::Bool)),
            },
            ExprId::new(2) => TypeConstraint::Dependent {
                constrained_by: ExprId::new(0),
                ptr: TypePtr::new(Type::Prim(ast::PrimType::Bool)),
            },
        }.into(),
    );
    assert!(logs.is_empty());
}

#[test]
fn constrain_var_bind_types() {
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
        table,
        hashmap! {
            ExprId::new(0) => TypeConstraint::Dependent {
                constrained_by: ExprId::new(2),
                ptr: TypePtr::new(Type::Prim(ast::PrimType::Bool)),
            },
            ExprId::new(1) => TypeConstraint::Independent {
                ptr: TypePtr::new(Type::Void),
            },
            ExprId::new(2) => TypeConstraint::Independent {
                ptr: TypePtr::new(Type::Prim(ast::PrimType::Bool)),
            },
        }.into(),
    );
    assert!(logs.is_empty());
}
