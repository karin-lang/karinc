use crate::parser::ast;

pub mod lower;

#[derive(Clone, Debug, PartialEq)]
pub struct Jsir {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub symbol: ast::GlobalSymbol,
    pub args: Vec<FormalArg>,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub id: ast::Id,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    VarDecl(VarDecl),
    VarDeclList(Vec<VarDecl>),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDecl {
    pub symbol: ast::LocalSymbol,
    pub init: Option<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {}
