use super::expr::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirItem {
    FunctionDeclaration(HirFunctionDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionDeclaration {
    pub id: HirId,
    pub exprs: Vec<HirExpression>,
}
