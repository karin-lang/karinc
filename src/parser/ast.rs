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
    pub kind: Box<ItemKind>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub id: Id,
    pub args: Vec<FormalArg>,
    pub ret_type: Option<Type>,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub id: Id,
    pub r#type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    VarDecl(VarDecl),
    VarInit(VarInit),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub kind: Box<TypeKind>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Prim(PrimType),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimType {
    Usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub id: Id,
    pub r#type: Option<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarInit {
    pub id: Id,
    pub r#type: Option<Type>,
    pub expr: Expr,
}
