#[cfg(test)]
mod expr;
mod item;

use maplit::hashmap;

use crate::{hir_def_id, hir_def_path};
use crate::data::{ast::*, token::*};
use crate::data::hir::{*, item::*};
use crate::hir::*;

#[test]
fn lowers_to_hir() {
    let ast = Ast::new(
        AstNode::new(
            "root".to_string(),
            vec![
                AstChild::node(
                    "fn_dec".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Id("f".to_string()), 0, 0),
                        ),
                        AstChild::node(
                            "fn_exprs".to_string(),
                            Vec::new(),
                        ),
                    ],
                ),
            ],
        ),
    );
    let ast_container = AstContainer {
        roots: vec![
            AstModule {
                path: vec!["my_hako".to_string()],
                ast,
                submodules: Vec::new(),
            },
        ],
    };
    let lowering = HirLowering::new();
    let (hir, logs) = lowering.lower(&ast_container);

    assert_eq!(
        hir,
        Hir {
            modules: hashmap! {
                hir_def_path!("my_hako") => (
                    HirModule {
                        items: hashmap! {
                            hir_def_id!("f") => (
                                HirItem::FunctionDeclaration(
                                    HirFunctionDeclaration {
                                        exprs: Vec::new(),
                                    },
                                )
                            ),
                        },
                        submodules: Vec::new(),
                    }
                ),
            },
        },
    );
    assert_eq!(logs, Vec::new());
}

#[test]
fn lowers_module() {
    let ast = Ast::new(
        AstNode::new(
            "root".to_string(),
            vec![
                AstChild::node(
                    "fn_dec".to_string(),
                    vec![
                        AstChild::leaf(
                            "id".to_string(),
                            Token::new(TokenKind::Id("f".to_string()), 0, 0),
                        ),
                        AstChild::node(
                            "fn_exprs".to_string(),
                            Vec::new(),
                        ),
                    ],
                ),
            ],
        ),
    );
    let ast_module = AstModule {
        path: vec!["my_hako".to_string()],
        ast,
        submodules: Vec::new(),
    };
    let mut lowering = HirLowering::new();
    let module = lowering.lower_module(&ast_module);

    assert_eq!(
        module,
        (
            (
                hir_def_path!("my_hako"),
                HirModule {
                    items: hashmap! {
                        hir_def_id!("f") => (
                            HirItem::FunctionDeclaration(
                                HirFunctionDeclaration {
                                    exprs: Vec::new(),
                                },
                            )
                        ),
                    },
                    submodules: Vec::new(),
                },
            ),
            Vec::new(),
        )
    );
    assert_eq!(lowering.logs, Vec::new());
}
