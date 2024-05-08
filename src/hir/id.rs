#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum LocalId {
    FormalArg(FormalArgId),
    Var(VarId),
}

impl std::fmt::Debug for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            LocalId::FormalArg(id) => format!("{:?}", id),
            LocalId::Var(id) => format!("{:?}", id),
        };
        write!(f, "{:?}", s)
    }
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TypeId {
    Item(ItemId),
    FormalArg(FormalArgId),
    Var(VarId),
    Expr(ExprId),
}

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TypeId::Item(id) => format!("{:?}", id),
            TypeId::FormalArg(id) => format!("{:?}", id),
            TypeId::Var(id) => format!("{:?}", id),
            TypeId::Expr(id) => format!("{:?}", id),
        };
        write!(f, "{:?}", s)
    }
}

pub trait NumId: Clone + Copy + std::fmt::Debug + Eq + std::hash::Hash + Ord + PartialEq + PartialOrd {
    fn new(id: usize) -> Self;

    fn into_usize(self) -> usize;
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct ItemId {
    id: usize,
}

impl NumId for ItemId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct VarId {
    id: usize,
}

impl NumId for VarId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct FormalArgId {
    id: usize,
}

impl NumId for FormalArgId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct ExprId {
    id: usize,
}

impl NumId for ExprId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}
