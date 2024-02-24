use super::path::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirExpression {
    Number(HirNumberLiteral),
    Symbol(HirSymbol),
    VariableDeclaration(HirVariableDeclaration),
    FunctionCall(HirFunctionCall),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirNumberLiteral {
    pub value: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirVariableDeclaration {
    pub symbol: HirSymbol,
    pub initial_expr: Option<Box<HirExpression>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionCall {
    pub symbol: HirSymbol,
    pub args: Vec<HirActualFunctionArgument>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirActualFunctionArgument {
    Expression(HirExpression),
}
