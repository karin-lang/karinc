pub mod id;
pub mod log;
pub mod lower;
pub mod resolve;

use std::collections::HashMap;

use crate::lexer::token;
use crate::parser::ast;
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: HashMap<ast::Path, Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub id: ItemId,
    pub accessibility: ast::Accessibility,
    pub kind: ItemKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub body: Body,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub ret_type: Option<Type>,
    pub args: Vec<FormalArgDef>,
    pub vars: Vec<VarDef>,
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(token::Literal),
    TopLevelRef(TopLevelId),
    LocalRef(LocalId),
    FnCall(FnCall),
    VarDef(VarId),
    VarBind(VarBind),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LocalDef {
    FormalArg(FormalArgDef),
    Var(VarDef),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub kind: Box<TypeKind>,
}

impl Type {
    pub fn new(kind: TypeKind) -> Type {
        Type { kind: Box::new(kind) }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    Item(ItemId),
    Prim(ast::PrimType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub r#fn: Option<ItemId>,
    pub args: Vec<ActualArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ActualArg {
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArgDef {
    pub id: FormalArgId,
    pub r#type: Type,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub ref_mut: ast::RefMut,
    pub r#type: Option<Type>,
    pub init: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarBind {
    pub var_id: VarId,
    pub value: Box<Expr>,
}
