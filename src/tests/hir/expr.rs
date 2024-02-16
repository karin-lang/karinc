use crate::data::{ast::*, token::*};
use crate::data::hir::expr::*;
use crate::hir::*;

#[test]
fn lowers_any_expression() {
    let child = AstChild::leaf(
        "number".to_string(),
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
    );
    let mut lowering = HirLowering::new();

    assert_eq!(
        lowering.lower_expression(&child),
        Some(
            HirExpression::Number(
                HirNumberLiteral {
                    value: "0".to_string(),
                }
            ),
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}

#[test]
fn lowers_number_literal() {
    let child = AstChild::leaf(
        "number".to_string(),
        Token::new(TokenKind::Number(NumberToken("0".to_string())), 0, 0),
    );
    let mut lowering = HirLowering::new();

    assert_eq!(
        lowering.lower_number_literal(&child),
        Some(
            HirNumberLiteral {
                value: "0".to_string(),
            }
        ),
    );
    assert_eq!(lowering.logs, Vec::new());
}
