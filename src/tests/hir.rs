#[cfg(test)]
mod expr;
mod item;

use crate::data::{ast::*, token::*};
use crate::data::hir::{expr::*, item::*, Hir};
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
    let lowering = HirLowering::new();
    let (hir, logs) = lowering.lower(&ast);

    assert_eq!(
        hir,
        Hir {
            items: vec![
                HirItem::FunctionDeclaration(
                    HirFunctionDeclaration {
                        id: HirId("f".to_string()),
                        exprs: Vec::new(),
                    },
                ),
            ],
        },
    );
    assert_eq!(logs, Vec::new());
}
