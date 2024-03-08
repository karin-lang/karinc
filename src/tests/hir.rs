use std::collections::HashMap;

use maplit::hashmap;

use crate::lexer::token::Span;
use crate::parser::ast;
use crate::hir::*;
use crate::hir::lowering::HirLowering;

#[test]
fn lowers_empty_ast() {
    let ast = ast::Ast { items: Vec::new() };
    let lowering = HirLowering::new(vec!["my_hako"].into());
    let (hir, logs) = lowering.lower(&ast);

    assert_eq!(
        hir,
        Hir { global_entities: HashMap::new() },
    );
    assert_eq!(logs, Vec::new());
}

#[test]
fn lowers_items_in_ast() {
    let ast = ast::Ast {
        items: vec![
            ast::Item {
                kind: Box::new(
                    ast::ItemKind::FnDecl(
                        ast::FnDecl {
                            id: ast::Id {
                                id: "f".to_string(),
                                span: Span::new(0, 1, 1),
                            },
                            args: Vec::new(),
                            ret_type: None,
                            body: Vec::new(),
                        },
                    ),
                ),
            },
        ],
    };
    let lowering = HirLowering::new(vec!["my_hako"].into());
    let (hir, logs) = lowering.lower(&ast);

    assert_eq!(
        hir,
        Hir {
            global_entities: hashmap! {
                vec!["my_hako", "f"].into() => (
                    GlobalEntity::FnDecl(
                        FnDecl {
                            entities: HashMap::new(),
                            args: Vec::new(),
                            body: Vec::new(),
                        },
                    )
                ),
            },
        },
    );
    assert_eq!(logs, Vec::new());
}
