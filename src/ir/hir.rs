#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: Vec<HirItem>,
}

impl Hir {
    pub fn new() -> Hir {
        Hir {
            items: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirNumber(pub String);

#[derive(Clone, Debug, PartialEq)]
pub struct HirIdentifier(pub String);

#[derive(Clone, Debug, PartialEq)]
pub enum HirItem {
    Function(HirFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunction {
    pub id: HirIdentifier,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirExpression {
    Number(HirNumber),
    Identifier(HirIdentifier),
    FunctionCall(HirFunctionCall),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunctionCall {
    pub id: HirIdentifier,
}
