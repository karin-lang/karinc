pub mod id;
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
pub struct DivPath {
    pub item_path: ast::Path,
    pub following_path: ast::Path,
}

impl DivPath {
    pub fn is_item(&self) -> bool {
        self.following_path.is_empty()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(token::Literal),
    FnCall(FnCall),
    PathRef(DivPath),
    VarDef(VarId),
    VarBind(VarBind),
    LocalRef(LocalId),
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
    Path(DivPath),
    Prim(ast::PrimType),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub r#fn: ast::Path,
    pub args: Vec<ActualArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ActualArg {
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArgDef {
    pub r#type: Type,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub r#type: Option<Type>,
    pub mutable: bool,
    pub init: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarBind {
    pub var_id: VarId,
    pub value: Box<Expr>,
}
