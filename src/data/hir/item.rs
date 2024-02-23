use super::expr::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirItem {
    FunctionDeclaration(HirFunctionDeclaration),
}

// todo: ローカル変数マップを追加する（型解析などに用いる）
// note: ローカル変数マップは名前解決コードと型などの変数情報を関連づける／変数定義用のstructは名前解決コードのみを持つ
// note: メンバ関数は別の構造体で管理する？DefIdをDefPathにして共通化する？
#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionDeclaration {
    pub exprs: Vec<HirExpression>,
}
