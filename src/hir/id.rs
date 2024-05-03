pub trait Id: Clone + Copy + std::fmt::Debug + Eq + std::hash::Hash + PartialEq {
    fn new(id: usize) -> Self;

    fn into_usize(self) -> usize;
}

#[derive(Clone, Debug, PartialEq)]
pub enum LocalId {
    FormalArg(FormalArgId),
    Var(VarId),
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, PartialEq)]
pub struct VarId {
    id: usize,
}

impl Id for VarId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, PartialEq)]
pub struct FormalArgId {
    id: usize,
}

impl Id for FormalArgId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, PartialEq)]
pub struct ExprId {
    id: usize,
}

impl Id for ExprId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}
