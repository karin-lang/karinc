#[cfg(test)]
mod ast;
#[cfg(test)]
mod hir;
#[cfg(test)]
mod lexer;
#[cfg(test)]
mod parser;
#[cfg(test)]
mod token;

use crate::hir::HirLowering;
use crate::{lexer::*, parser::*};
use crate::data::{ast::*, token::*};
use crate::data::hir::{*, expr::*, item::*};

#[test]
fn generates_parser_result() {
    let input = "fn f(){0;}";
    let lexer = Lexer::new();
    let (tokens, lexer_logs) = lexer.tokenize(input);

    assert_eq!(lexer_logs, Vec::new());

    let parser = Parser::new();
    let (parser_result, parser_logs) = parser.parse(&tokens);

    assert_eq!(
        parser_result,
        ParserResult::Matched(
            Some(
                Ast::new(
                    AstNode::new(
                        "root".to_string(),
                        vec![
                            AstChild::node(
                                "fn_dec".to_string(),
                                vec![
                                    AstChild::leaf(
                                        "id".to_string(),
                                        Token::new(TokenKind::Id("f".to_string()), 3, 1),
                                    ),
                                    AstChild::node(
                                        "fn_exprs".to_string(),
                                        vec![
                                            AstChild::leaf(
                                                "number".to_string(),
                                                Token::new(TokenKind::Number(NumberToken("0".to_string())), 7, 1),
                                            ),
                                        ],
                                    ),
                                ],
                            ),
                        ],
                    ),
                ),
            ),
        ),
    );
    assert_eq!(parser_logs, Vec::new());

    let ast = parser_result.unwrap().unwrap();
    let hir_lowering = HirLowering::new();
    let (hir, hir_lowering_logs) = hir_lowering.lower(&ast);

    assert_eq!(
        hir,
        Hir {
            items: vec![
                HirItem::FunctionDeclaration(
                    HirFunctionDeclaration {
                        id: HirId("f".to_string()),
                        exprs: vec![
                            HirExpression::Number(
                                HirNumberLiteral {
                                    value: "0".to_string(),
                                },
                            ),
                        ],
                    },
                ),
            ],
        },
    );
    assert_eq!(
        hir_lowering_logs,
        Vec::new(),
    );
}
