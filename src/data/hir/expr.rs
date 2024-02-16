#[derive(Clone, Debug, PartialEq)]
pub struct HirId(pub String);

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
    pub id: HirId,
    pub args: Vec<HirActualFunctionArgument>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirActualFunctionArgument {
    Expression(HirExpression),
}
