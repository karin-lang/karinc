#[cfg(test)]
mod lexer;
#[cfg(test)]
mod parser;
/*
#[cfg(test)]
mod hir;
#[cfg(test)]
mod parser;
#[cfg(test)]
mod token;
*/

/*
use maplit::hashmap;

use crate::hir::HirLowering;
use crate::{lexer::*, parser::*};
use crate::data::{ast::*, token::*};
use crate::data::hir::{*, expr::*, item::*};

#[macro_export]
macro_rules! hir_global_symbol {
    ($($segment:expr,)+) => {
        hir_global_symbol!($($segment),+)
    };

    ($($segment:expr),*) => {
        {
            use crate::data::hir::symbol::*;

            HirGlobalSymbol {
                segments: vec![$($segment.to_string()),*],
            }
        }
    };
}

#[macro_export]
macro_rules! hir_divided_global_symbol {
    ([$($parent_module_segment:expr,)+], [$($following_segment:expr,)+]$(,)?) => {
        hir_divided_global_symbol!([$($parent_module_segment:expr),+], [$($following_segment),+])
    };

    ([$($parent_module_segment:expr),*], [$($following_segment:expr),*$(,)?]) => {
        {
            use crate::data::hir::symbol::*;
            let parent_segments = vec![$($parent_module_segment.to_string()),*];

            HirDividedGlobalSymbol {
                parent_module_path: HirPath { segments: parent_segments },
                following_path: HirPath { segments: vec![$($following_segment.to_string()),*] },
            }
        }
    };
}

#[macro_export]
macro_rules! hir_local_symbol {
    ($id:expr, $code:expr$(,)?) => {
        {
            use crate::data::hir::symbol::*;

            HirLocalSymbol {
                id: $id.to_string(),
                code: HirSymbolCode::new($code),
            }
        }
    };
}

#[macro_export]
macro_rules! hir_symbol_accessor {
    ($segment:expr, $code:expr$(,)?) => {
        {
            use crate::data::hir::symbol::*;

            HirSymbolAccessor {
                index: HirSymbolIndex::new($code),
                kind: HirSymbolAccessorKind::SingleSegment($segment.to_string()),
            }
        }
    };

    ([$($path_segment:expr),*], [$($member_access_chain_segment:expr),*], $code:expr$(,)?) => {
        {
            use crate::data::hir::symbol::*;

            HirSymbolAccessor {
                index: HirSymbolIndex::new($code),
                kind: HirSymbolAccessorKind::MultipleSegments(
                    vec![$($path_segment.to_string()),*],
                    vec![$($member_access_chain_segment.to_string()),*],
                ),
            }
        }
    };
}

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

    let ast_container = AstContainer {
        roots: vec![
            AstModule {
                path: vec!["my_hako".to_string()],
                ast,
                submodules: Vec::new(),
            },
        ],
    };

    let hir_lowering = HirLowering::new();
    let (hir, hir_lowering_logs) = hir_lowering.lower(&ast_container);

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
                                        exprs: vec![
                                            HirExpression::Number(
                                                HirNumberLiteral {
                                                    value: "0".to_string(),
                                                },
                                            ),
                                        ],
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
    assert_eq!(
        hir_lowering_logs,
        Vec::new(),
    );
}
*/
