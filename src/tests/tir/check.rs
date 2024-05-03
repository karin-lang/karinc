use maplit::hashmap;

use crate::parser::ast;
use crate::tir::{*, check::*};

#[test]
fn check_tir() {
    let tir = Tir {
        bodies: hashmap! {
            "my_hako::f".into() => (
                Body {
                    locals: vec![
                        Local::FormalArg(
                            FormalArg {
                                r#type: Type::new(TypeKind::Prim(ast::PrimType::Bool)),
                            },
                        ),
                        Local::VarInit(
                            VarInit {
                                r#type: Type::new(TypeKind::Prim(ast::PrimType::Str)),
                                init: Expr {
                                    kind: ExprKind::LocalRef(0.into()),
                                },
                            },
                        ),
                    ],
                    exprs: vec![
                        Expr {
                            kind: ExprKind::LocalDecl(1.into()),
                        },
                    ],
                }
            ),
        },
    };
    let check = TypeCheck::new();

    assert_eq!(
        check.check(&tir),
        vec![TypeCheckLog::TypeUnmatch],
    );
}
