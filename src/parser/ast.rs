#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub items: Vec<Item>,
}

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct Id(String);

impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl From<String> for Id {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&String> for Id {
    fn from(value: &String) -> Self {
        Self(value.clone())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub id: Id,
    pub args: Vec<FormalArg>,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub id: Id,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    VarInit(VarInit),
    VarDecl(VarDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarInit {
    pub id: Id,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
}
