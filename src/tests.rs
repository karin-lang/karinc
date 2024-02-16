#[cfg(test)]
mod hir;
#[cfg(test)]
mod lexer;
#[cfg(test)]
mod parser;
#[cfg(test)]
mod token;

use crate::{lexer::*, parser::*};
use crate::data::{ast::*, token::*};

#[test]
fn generates_parser_result() {
    let input = "fn f(){}";
    let lexer = Lexer::new();
    let (tokens, lexer_logs) = lexer.tokenize(input);

    assert_eq!(lexer_logs, Vec::new());

    let parser = Parser::new();
    let (parser_result, parser_logs) = parser.parse(&tokens);

    assert_eq!(parser_logs, Vec::new());
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
                                        Token::new(TokenKind::Identifier("f".to_string()), 3, 1),
                                    ),
                                    AstChild::node(
                                        "fn_exprs".to_string(),
                                        Vec::new(),
                                    ),
                                ],
                            ),
                        ],
                    ),
                ),
            ),
        ),
    );
}
