use super::expr::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirItem {
    FunctionDeclaration(HirFunctionDeclaration),
}

// note: メンバ関数は別の構造体で管理する
#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionDeclaration {
    pub exprs: Vec<HirExpression>,
}
