pub mod id;
pub mod log;
pub mod lower;
pub mod resolve;

use std::collections::HashMap;

use crate::lexer::token::{self, Span};
use crate::parser::ast::{self, Id};
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: HashMap<ast::Path, Item>,
    pub todos: Vec<(String, Span)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub id: ItemId,
    pub mod_id: ModId,
    pub marker: MarkerInfo,
    pub accessibility: ast::Accessibility,
    pub kind: ItemKind,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MarkerInfo {
    pub sys_embed: Option<String>,
    pub spec_description: Option<String>,
    pub arg_descriptions: HashMap<String, String>,
    pub ret_val_description: Option<String>,
    pub todos: Vec<(String, bool)>,
}

impl MarkerInfo {
    pub fn new() -> MarkerInfo {
        MarkerInfo {
            sys_embed: None,
            spec_description: None,
            arg_descriptions: HashMap::new(),
            ret_val_description: None,
            todos: Vec::new(),
        }
    }
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
    pub id: BodyId,
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
    Operation(Box<Operation>),
    Block(Block),
    Literal(token::Literal),
    TopLevelRef(TopLevelId, ast::Path),
    LocalRef(LocalId),
    Ret(Ret),
    FnCall(FnCall),
    VarDef(VarId),
    VarBind(VarBind),
    If(If),
    For(For),
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    Unary { operator: ast::UnaryOperator, term: Expr },
    Binary { operator: ast::BinaryOperator, left_term: Expr, right_term: Expr },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub exprs: Vec<Expr>,
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
pub struct Ret {
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub r#fn: Option<(ItemId, ast::Path)>,
    pub args: Vec<ActualArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ActualArg {
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArgDef {
    pub id: FormalArgId,
    pub ref_mut: ast::RefMut,
    pub r#type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub id: Id,
    pub ref_mut: ast::RefMut,
    pub r#type: Option<Type>,
    pub init: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarBind {
    pub var_id: VarId,
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub cond: Box<Expr>,
    pub block: Block,
    pub elifs: Vec<Elif>,
    pub r#else: Option<Block>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Elif {
    pub cond: Box<Expr>,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct For {
    pub kind: ForKind,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForKind {
    Endless,
    Cond { cond: Box<Expr> },
    Range { index: Box<Expr>, range: Box<Expr> },
}
