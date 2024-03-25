#[cfg(test)]
mod hir;
#[cfg(test)]
mod lexer;
#[cfg(test)]
mod parser;
#[cfg(test)]
mod token;

use maplit::hashmap;

use crate::hir::lower::HirLowering;
use crate::lexer::token::Span;
use crate::parser::{ast, Parser};
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

    let parser = Parser::new(&tokens);
    let (ast, parser_logs) = parser.parse("my_hako".into());
    assert_eq!(
        ast,
        ast::Ast {
            mod_path: "my_hako".into(),
            items: vec![
                ast::Item {
                    id: ast::Id { id: "f".to_string(), span: Span::new(3, 1) },
                    kind: ast::ItemKind::FnDecl(
                        ast::FnDecl {
                            args: Vec::new(),
                            ret_type: None,
                            body: ast::Body {
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

    let asts = vec![ast];
    let lowering = HirLowering::new(&asts);
    let (hir, hir_lowering_logs) = lowering.lower();
    assert_eq!(
        hir,
        crate::hir::Hir {
            items: hashmap! {
                "my_hako::f".into() => (
                    crate::hir::Item::FnDecl(
                        crate::hir::FnDecl {
                            args: Vec::new(),
                            body: crate::hir::Body {
                                locals: Vec::new(),
                                exprs: vec![crate::hir::Expr::PathRef("my_hako::f".into())],
                            },
                        },
                    )
                ),
            },
        },
    );
    assert!(hir_lowering_logs.is_empty());
}
