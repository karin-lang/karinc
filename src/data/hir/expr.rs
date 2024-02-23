use super::path::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirExpression {
    Number(HirNumberLiteral),
    FunctionCall(HirFunctionCall),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirNumberLiteral {
    pub value: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionCall {
    pub id: HirRefPath,
    pub args: Vec<HirActualFunctionArgument>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirActualFunctionArgument {
    Expression(HirExpression),
}
