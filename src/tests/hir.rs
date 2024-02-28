#[cfg(test)]
mod expr;
#[cfg(test)]
mod item;

use std::collections::HashMap;

use maplit::hashmap;

use crate::*;
use crate::data::{ast::*, token::*};
use crate::data::hir::{*, expr::*, item::*, symbol::*};
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
                hir_divided_global_symbol!([], ["my_hako"]) => (
                    HirModule {
                        items: hashmap! {
                            hir_divided_global_symbol!([], ["my_hako", "f"]) => (
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
    let modules = lowering.lower_module(&ast_module);

    assert_eq!(
        modules,
        (
            (
                hir_divided_global_symbol!([], ["my_hako"]),
                HirModule {
                    items: hashmap! {
                        hir_divided_global_symbol!([], ["my_hako", "f"]) => (
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
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn separates_submodule_result_into_first_and_the_following() {
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
        ast: ast.clone(),
        submodules: vec![
            AstModule {
                path: vec!["my_hako".to_string(), "submodule1".to_string()],
                ast: ast.clone(),
                submodules: Vec::new(),
            },
            AstModule {
                path: vec!["my_hako".to_string(), "submodule2".to_string()],
                ast,
                submodules: Vec::new(),
            },
        ],
    };
    let mut lowering = HirLowering::new();
    let modules = lowering.lower_module(&ast_module);

    let expected_items = |symbol: HirDividedGlobalSymbol| {
        let mut items = HashMap::new();

        items.insert(
            symbol,
            HirItem::FunctionDeclaration(
                HirFunctionDeclaration {
                    exprs: Vec::new(),
                },
            ),
        );

        items
    };

    assert_eq!(
        modules,
        (
            (
                hir_divided_global_symbol!([], ["my_hako"]),
                HirModule {
                    items: expected_items(hir_divided_global_symbol!([], ["my_hako", "f"])),
                    submodules: vec![
                        hir_divided_global_symbol!(["my_hako"], ["submodule1"]),
                        hir_divided_global_symbol!(["my_hako"], ["submodule2"]),
                    ],
                },
            ),
            vec![
                (
                    hir_divided_global_symbol!(["my_hako"], ["submodule1"]),
                    HirModule {
                        items: expected_items(hir_divided_global_symbol!(["my_hako"], ["submodule1", "f"])),
                        submodules: Vec::new(),
                    },
                ),
                (
                    hir_divided_global_symbol!(["my_hako"], ["submodule2"]),
                    HirModule {
                        items: expected_items(hir_divided_global_symbol!(["my_hako"], ["submodule2", "f"])),
                        submodules: Vec::new(),
                    },
                ),
            ],
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn increments_symbol_index_for_each_module_context() {
    let function_declaration_node = AstChild::node(
        "fn_dec".to_string(),
        vec![
            AstChild::leaf(
                "id".to_string(),
                Token::new(TokenKind::Id("f".to_string()), 0, 0),
            ),
            AstChild::node(
                "fn_exprs".to_string(),
                vec![
                    AstChild::node(
                        "id_or_path".to_string(),
                        vec![
                            AstChild::leaf(
                                "id".to_string(),
                                Token::new(TokenKind::Id("id1".to_string()), 0, 1),
                            ),
                        ],
                    ),
                    AstChild::node(
                        "id_or_path".to_string(),
                        vec![
                            AstChild::leaf(
                                "id".to_string(),
                                Token::new(TokenKind::Id("id2".to_string()), 1, 1),
                            ),
                        ],
                    ),
                ],
            ),
        ],
    );
    let ast = Ast::new(
        AstNode::new(
            "root".to_string(),
            vec![function_declaration_node],
        ),
    );
    let ast_module = AstModule {
        path: vec!["my_hako".to_string()],
        ast: ast.clone(),
        submodules: vec![
            AstModule {
                path: vec!["my_hako".to_string(), "submodule".to_string()],
                ast: ast,
                submodules: Vec::new(),
            },
        ],
    };
    let mut lowering = HirLowering::new();
    let modules = lowering.lower_module(&ast_module);

    let expected_items = |symbol: HirDividedGlobalSymbol| {
        let mut items = HashMap::new();

        items.insert(
            symbol,
            HirItem::FunctionDeclaration(
                HirFunctionDeclaration {
                    exprs: vec![
                        HirExpression::Symbol(hir_symbol_accessor!(["id1"], 0)),
                        HirExpression::Symbol(hir_symbol_accessor!(["id2"], 1)),
                    ],
                },
            ),
        );

        items
    };

    assert_eq!(
        modules,
        (
            (
                hir_divided_global_symbol!([], ["my_hako"]),
                HirModule {
                    items: expected_items(hir_divided_global_symbol!([], ["my_hako", "f"])),
                    submodules: vec![
                        hir_divided_global_symbol!(["my_hako"], ["submodule"]),
                    ],
                },
            ),
            vec![
                (
                    hir_divided_global_symbol!(["my_hako"], ["submodule"]),
                    HirModule {
                        items: expected_items(hir_divided_global_symbol!(["my_hako"], ["submodule", "f"])),
                        submodules: Vec::new(),
                    },
                ),
            ],
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_modules_in_all_layers_of_hierarchy() {
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
        ast: ast.clone(),
        submodules: vec![
            AstModule {
                path: vec!["my_hako".to_string(), "submodule1".to_string()],
                ast: ast.clone(),
                submodules: vec![
                    AstModule {
                        path: vec!["my_hako".to_string(), "submodule1".to_string(), "submodule1_1".to_string()],
                        ast,
                        submodules: Vec::new(),
                    },
                ],
            },
        ],
    };
    let mut lowering = HirLowering::new();
    let modules = lowering.lower_module(&ast_module);

    let expected_items = |symbol: HirDividedGlobalSymbol| {
        let mut items = HashMap::new();

        items.insert(
            symbol,
            HirItem::FunctionDeclaration(
                HirFunctionDeclaration {
                    exprs: Vec::new(),
                },
            ),
        );

        items
    };

    assert_eq!(
        modules,
        (
            (
                hir_divided_global_symbol!([], ["my_hako"]),
                HirModule {
                    items: expected_items(hir_divided_global_symbol!([], ["my_hako", "f"])),
                    submodules: vec![
                        hir_divided_global_symbol!(["my_hako"], ["submodule1"]),
                    ],
                },
            ),
            vec![
                (
                    hir_divided_global_symbol!(["my_hako"], ["submodule1"]),
                    HirModule {
                        items: expected_items(hir_divided_global_symbol!(["my_hako"], ["submodule1", "f"])),
                        submodules: vec![
                            hir_divided_global_symbol!(["my_hako", "submodule1"], ["submodule1_1"]),
                        ],
                    },
                ),
                (
                    hir_divided_global_symbol!(["my_hako", "submodule1"], ["submodule1_1"]),
                    HirModule {
                        items: expected_items(hir_divided_global_symbol!(["my_hako", "submodule1"], ["submodule1_1", "f"])),
                        submodules: Vec::new(),
                    },
                ),
            ],
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}
