use std::fmt;

use super::Span;

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct NodeId {
    id: usize,
}

impl From<usize> for NodeId {
    fn from(value: usize) -> Self {
        Self { id: value }
    }
}

impl Into<usize> for NodeId {
    fn into(self) -> usize {
        self.id
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NodeIdGen {
    id: usize,
}

impl NodeIdGen {
    pub fn new() -> NodeIdGen {
        NodeIdGen { id: 0 }
    }

    pub fn generate(&mut self) -> NodeId {
        let new = self.id.into();
        self.id += 1;
        new
    }
}

// todo: パーサ呼び出し部分でモジュール情報付きの Hako を生成する
#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub items: Vec<Item>,
    pub hako_mods: Vec<NodeId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub node_id: NodeId,
    pub id: Id,
    pub kind: ItemKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    Mod(Mod),
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mod {
    pub submods: Vec<NodeId>,
    pub items: Vec<(String, NodeId)>,
}

#[derive(Clone, PartialEq)]
pub struct Id {
    pub id: String,
    pub span: Span,
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id({:?}, {:?})", self.id, self.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub args: Vec<FormalArg>,
    pub ret_type: Option<Type>,
    pub body: Body,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub id: Id,
    pub r#type: Type,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Id(Id),
    FormalArg(Id),
    VarDecl(VarDecl),
    VarInit(VarInit),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub kind: Box<TypeKind>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Id(Id),
    Prim(PrimType),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PrimType {
    Bool,
    I8, I16, I32, I64, Isize,
    U8, U16, U32, U64, Usize,
    F32, F64,
    Char, Str,
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
    pub expr: Box<Expr>,
}
