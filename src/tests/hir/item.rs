use std::collections::HashMap;

use crate::lexer::token::{self, Span};
use crate::parser::ast::{self, Id};
use crate::hir::*;
use crate::hir::id::*;
use crate::hir::lower::HirLowering;

#[test]
fn lowers_fn_decl() {
    let ast = ast::FnDecl {
        body: ast::Body {
            id: BodyId::new(0),
            ret_type: None,
            args: Vec::new(),
            exprs: Vec::new(),
        },
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_fn_decl(&ast);

    assert_eq!(
        hir,
        FnDecl {
            body: Body {
                id: BodyId::new(0),
                ret_type: None,
                args: Vec::new(),
                vars: Vec::new(),
                exprs: Vec::new(),
            },
        },
    );
    assert!(lowering.get_logs().is_empty());
}

#[test]
fn lowers_body() {
    let ast = ast::Body {
        id: BodyId::new(0),
        ret_type: Some(
            ast::Type {
                kind: Box::new(ast::TypeKind::Prim(ast::PrimType::Bool)),
                span: Span::new(0, 1),
            },
        ),
        args: vec![
            ast::FormalArg {
                id: Id { id: "arg".to_string(), span: Span::new(1, 1) },
                ref_mut: ast::RefMut::None,
                r#type: ast::Type {
                    kind: Box::new(ast::TypeKind::Prim(ast::PrimType::Bool)),
                    span: Span::new(2, 1),
                },
            },
        ],
        exprs: vec![
            ast::Expr {
                kind: ast::ExprKind::Literal(
                    token::Literal::Bool { value: true },
                ),
                span: Span::new(3, 1),
            },
        ],
    };
    let asts = Vec::new();
    let paths = HashMap::new();
    let mut lowering = HirLowering::new(&asts);
    lowering.debug_in_body(paths);
    let hir = lowering.lower_body(&ast);

    assert_eq!(
        hir,
        Body {
            id: BodyId::new(0),
            ret_type: Some(
                Type {
                    kind: Box::new(TypeKind::Prim(ast::PrimType::Bool)),
                },
            ),
            args: vec![
                FormalArgDef {
                    id: FormalArgId::new(0),
                    ref_mut: ast::RefMut::None,
                    r#type: Type {
                        kind: Box::new(TypeKind::Prim(ast::PrimType::Bool)),
                    },
                },
            ],
            vars: Vec::new(),
            exprs: vec![
                Expr {
                    id: ExprId::new(0),
                    kind: ExprKind::Literal(
                        token::Literal::Bool { value: true },
                    ),
                },
            ],
        },
    );
    assert!(lowering.get_logs().is_empty());
}
