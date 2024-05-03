use maplit::hashmap;

use crate::hir;
use crate::parser::ast;
use crate::tir::{*, lower::*};

#[test]
fn lower_hir_to_tir() {
    let hir = hir::Hir {
        items: hashmap! {
            "my_hako::f".into() => (
                hir::Item::FnDecl(
                    hir::FnDecl {
                        args: vec![0.into()],
                        body: hir::Body {
                            locals: vec![
                                hir::Local::FormalArg(
                                    hir::FormalArg {
                                        r#type: hir::Type {
                                            kind: Box::new(hir::TypeKind::Prim(ast::PrimType::Bool)),
                                        },
                                        mutable: false,
                                    },
                                ),
                            ],
                            exprs: vec![hir::Expr::LocalRef(0.into())],
                        },
                    },
                )
            ),
        },
    };
    let lowering = TirLowering::new(&hir);

    assert_eq!(
        lowering.lower(),
        Tir {
            bodies: hashmap! {
                "my_hako::f".into() => (
                    Body {
                        locals: vec![
                            Local::FormalArg(
                                FormalArg {
                                    r#type: Type::new(TypeKind::Prim(ast::PrimType::Bool)),
                                },
                            ),
                        ],
                        exprs: vec![
                            Expr {
                                kind: ExprKind::LocalRef(0.into()),
                            },
                        ],
                    }
                ),
            },
        },
    );
}
