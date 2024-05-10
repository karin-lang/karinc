use maplit::hashmap;

use crate::hir::{self, id::*};
use crate::lexer::token::Span;
use crate::parser::ast::tltype;
use crate::parser::{self, ast, ParserHakoContext};
use crate::tir::constraint;
use crate::{id_token, keyword_token, token};
use crate::lexer::tokenize::Lexer;

#[test]
fn generates_js() {
    let input = "fn f() { f; }";

    let lexer = Lexer::new();
    let (tokens, lexer_logs) =  lexer.tokenize(input);
    assert_eq!(
        tokens,
        vec![
            keyword_token!(Fn, 0, 2),
            id_token!("f", 3, 1),
            token!(OpenParen, 4, 1),
            token!(ClosingParen, 5, 1),
            token!(OpenCurlyBracket, 7, 1),
            id_token!("f", 9, 1),
            token!(Semicolon, 10, 1),
            token!(ClosingCurlyBracket, 12, 1),
        ],
    );
    assert!(lexer_logs.is_empty());

    let mut hako_context = ParserHakoContext::new(HakoId::new(0));
    let parser = parser::Parser::new(&tokens, &mut hako_context);
    let (ast, parser_logs) = parser.parse(ModId::new(0, 0), "my_hako".into());
    assert_eq!(
        ast,
        ast::Ast {
            mod_id: ModId::new(0, 0),
            mod_path: "my_hako".into(),
            items: vec![
                ast::Item {
                    id: ItemId::new(0, 0),
                    name: ast::Id { id: "f".to_string(), span: Span::new(3, 1) },
                    kind: ast::ItemKind::FnDecl(
                        ast::FnDecl {
                            body: ast::Body {
                                ret_type: None,
                                args: Vec::new(),
                                exprs: vec![
                                    ast::Expr {
                                        kind: ast::ExprKind::Id(
                                            ast::Id { id: "f".to_string(), span: Span::new(9, 1) },
                                        ),
                                        span: Span::new(9, 1),
                                    },
                                ],
                            },
                        },
                    ),
                },
            ],
        },
    );
    assert!(parser_logs.is_empty());

    let asts = vec![&ast];
    let lowering = hir::lower::HirLowering::new(&asts);
    let (hir, hir_lowering_logs) = lowering.lower();
    assert_eq!(
        hir,
        hir::Hir {
            items: hashmap! {
                "my_hako::f".into() => (
                    hir::Item {
                        id: ItemId::new(0, 0),
                        kind: hir::ItemKind::FnDecl(
                            hir::FnDecl {
                                body: hir::Body {
                                    ret_type: None,
                                    args: Vec::new(),
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
        },
    );
    assert!(hir_lowering_logs.is_empty());

    let mut top_level_type_table = tltype::TopLevelTypeTable::new();
    top_level_type_table.absorb(&ast);
    assert_eq!(
        top_level_type_table,
        hashmap! {
            TopLevelId::Item(ItemId::new(0, 0)) => constraint::Type::Fn {
                ret_type: Box::new(constraint::Type::Void),
                arg_types: Vec::new(),
            },
        }.into(),
    );

    let (
        type_constraint_table,
        type_constraint_lowering_logs,
    ) = constraint::TypeConstraintLowering::lower(&hir, &top_level_type_table);
    assert_eq!(
        type_constraint_table.to_sorted_vec(),
        constraint::TypeConstraintTable::from(
            hashmap! {
                TypeId::Expr(ExprId::new(0)) => constraint::TypeConstraint::new_constrained(
                    constraint::TypePtr::new(
                        constraint::Type::Fn {
                            ret_type: Box::new(constraint::Type::Void),
                            arg_types: Vec::new(),
                        },
                    ),
                    Vec::new(),
                    Some(TypeId::TopLevel(TopLevelId::Item(ItemId::new(0, 0)))),
                ),
            },
        ).to_sorted_vec(),
    );
    assert!(type_constraint_lowering_logs.is_empty());
}
