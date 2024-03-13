use std::collections::HashMap;

use super::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast {
    pub global_symbol_table: GlobalSymbolTable,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GlobalSymbolTable {
    table: HashMap<GlobalSymbol, GlobalEntity>,
}

impl GlobalSymbolTable {
    pub fn new() -> GlobalSymbolTable {
        GlobalSymbolTable { table: HashMap::new() }
    }
}

impl GlobalSymbolTable {
    pub fn insert(&mut self, symbol: GlobalSymbol, entity: GlobalEntity) -> Option<GlobalEntity> {
        self.table.insert(symbol, entity)
    }
}

impl From<HashMap<GlobalSymbol, GlobalEntity>> for GlobalSymbolTable {
    fn from(value: HashMap<GlobalSymbol, GlobalEntity>) -> Self {
        Self { table: value }
    }
}

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct GlobalSymbol {
    segments: Vec<String>,
}

impl GlobalSymbol {
    pub fn add(mut self, segment: &str) -> GlobalSymbol {
        self.segments.push(segment.to_string());
        self
    }
}

impl From<Vec<&str>> for GlobalSymbol {
    fn from(value: Vec<&str>) -> Self {
        Self { segments: value.iter().map(|v| v.to_string()).collect() }
    }
}

impl From<Vec<String>> for GlobalSymbol {
    fn from(value: Vec<String>) -> Self {
        Self { segments: value }
    }
}

impl From<Vec<&String>> for GlobalSymbol {
    fn from(value: Vec<&String>) -> Self {
        Self { segments: value.iter().map(|v| (*v).clone()).collect() }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum GlobalEntity {
    // todo: モジュール構造を追加
    Module,
    FnDecl(FnDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalSymbolTable {
    table: HashMap<LocalSymbol, LocalEntity>,
}

impl LocalSymbolTable {
    pub fn new() -> LocalSymbolTable {
        LocalSymbolTable { table: HashMap::new() }
    }

    pub fn insert(&mut self, symbol: LocalSymbol, entity: LocalEntity) -> Option<LocalEntity> {
        self.table.insert(symbol, entity)
    }
}

#[derive(Clone, Eq, Debug, Hash, PartialEq)]
pub struct LocalSymbol {
    index: usize,
}

impl From<usize> for LocalSymbol {
    fn from(value: usize) -> Self {
        Self { index: value }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LocalEntity {
    FormalArg(Id),
    VarDecl(VarDecl),
    VarInit(VarInit),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Id {
    pub id: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl {
    pub id: Id,
    pub args: Vec<FormalArg>,
    pub ret_type: Option<Type>,
    pub body: Vec<Expr>,
    pub symbol_table: LocalSymbolTable,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalArg {
    pub id: Id,
    pub r#type: Type,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Id(Id, Option<LocalSymbol>),
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
    pub expr: Expr,
}
