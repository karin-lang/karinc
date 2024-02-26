use super::symbol::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirExpression {
    Number(HirNumberLiteral),
    Symbol(HirSymbolAccessor),
    VariableDeclaration(HirVariableDeclaration),
    FunctionCall(HirFunctionCall),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirNumberLiteral {
    pub value: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirVariableDeclaration {
    pub symbol: HirLocalSymbol,
    pub initial_expr: Option<Box<HirExpression>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionCall {
    pub symbol: HirSymbolAccessor,
    pub args: Vec<HirActualFunctionArgument>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirActualFunctionArgument {
    Expression(HirExpression),
}
