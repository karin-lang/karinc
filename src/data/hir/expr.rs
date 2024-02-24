use super::path::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirExpression {
    Number(HirNumberLiteral),
    NameResolutionTarget(HirRefIdOrPath),
    VariableDeclaration(HirVariableDeclaration),
    FunctionCall(HirFunctionCall),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirNumberLiteral {
    pub value: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirVariableDeclaration {
    pub code: HirDefLocalCode,
    pub initial_expr: Option<Box<HirExpression>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionCall {
    pub id: HirRefIdOrPath,
    pub args: Vec<HirActualFunctionArgument>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirActualFunctionArgument {
    Expression(HirExpression),
}
