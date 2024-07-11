pub mod tltype;

use std::fmt;

use crate::lexer::token::{self, Token, TokenKind};
use crate::hir::id::*;
use crate::parser::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub mod_id: ModId,
    pub mod_path: Path,
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Path {
    pub segments: Vec<String>,
}

impl Path {
    pub fn new() -> Path {
        Path { segments: Vec::new() }
    }

    pub fn from(segments: Vec<String>) -> Path {
        Path { segments }
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn add_segment(mut self, segment: &str) -> Path {
        self.segments.push(segment.to_string());
        self
    }

    pub fn push_segment(&mut self, segment: &str) {
        self.segments.push(segment.to_string());
    }

    pub fn pop_segment(&mut self) -> Option<String> {
        self.segments.pop()
    }
}

impl From<&str> for Path {
    fn from(value: &str) -> Self {
        Self { segments: value.split("::").map(|v| v.to_string()).collect() }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Accessibility {
    Default,
    Pub,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    pub id: ItemId,
    pub name: Id,
    pub accessibility: Accessibility,
    pub kind: ItemKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ItemKind {
    FnDecl(FnDecl),
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
    pub body: Body,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    pub id: BodyId,
    pub ret_type: Option<Type>,
    pub args: Vec<FormalArg>,
    pub exprs: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub id: Id,
    pub ref_mut: RefMut,
    pub r#type: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Operation(Box<Operation>),
    Block(Block),
    Id(Id),
    Path(Path),
    Literal(token::Literal),
    Ret(Ret),
    FnCall(FnCall),
    VarDef(VarDef),
    VarBind(VarBind),
    If(If),
    For(For),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Operation {
    pub elems: Vec<OperationElem>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum OperationElem {
    Term(Expr),
    Operator(Operator),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operator {
    pub fn get_precedence(&self) -> usize {
        match self {
            Operator::Add => 0,
            Operator::Sub => 0,
            Operator::Mul => 1,
            Operator::Div => 1,
        }
    }

    pub fn to_infix_operator(token: &Token) -> Option<Operator> {
        let op = match &token.kind {
            TokenKind::Plus => Operator::Add,
            TokenKind::Minus => Operator::Sub,
            TokenKind::Asterisk => Operator::Mul,
            TokenKind::Slash => Operator::Div,
            _ => return None,
        };
        return Some(op);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub exprs: Vec<Expr>,
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
    Void,
    Bool,
    I8, I16, I32, I64, Isize,
    U8, U16, U32, U64, Usize,
    F32, F64,
    Char, Str,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ret {
    pub value: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall {
    pub path: Path,
    pub args: Vec<ActualArg>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ActualArg {
    pub ref_mut: RefMut,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub id: Id,
    pub ref_mut: RefMut,
    pub r#type: Option<Type>,
    pub init: Option<Box<Expr>>,
}

// referability and mutability of variable
#[derive(Clone, Debug, PartialEq)]
pub enum RefMut {
    None,
    Ref,
    Mut,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarBind {
    pub id: Id,
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
