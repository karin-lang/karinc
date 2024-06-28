#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum GlobalId {
    Hako(HakoId),
    Mod(ModId),
    Item(ItemId),
    ItemMember(ItemMemberId),
}

impl std::fmt::Debug for GlobalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            GlobalId::Hako(id) => format!("{:?}", id),
            GlobalId::Mod(id) => format!("{:?}", id),
            GlobalId::Item(id) => format!("{:?}", id),
            GlobalId::ItemMember(id) => format!("{:?}", id),
        };
        write!(f, "{:?}", s)
    }
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TopLevelId {
    Item(ItemId),
    ItemMember(ItemMemberId),
    FnRet(ItemId),
    FnArg(ItemId, FormalArgId),
}

impl std::fmt::Debug for TopLevelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TopLevelId::Item(id) => format!("{:?}", id),
            TopLevelId::ItemMember(id) => format!("{:?}", id),
            TopLevelId::FnRet(id) => format!("FnRet({:?})", id),
            TopLevelId::FnArg(item_id, formal_arg_id) => format!("FnArg({:?}, {:?})", item_id, formal_arg_id),
        };
        write!(f, "{:?}", s)
    }
}

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
    TopLevel(TopLevelId),
    FormalArg(BodyId, FormalArgId),
    Var(BodyId, VarId),
    Expr(BodyId, ExprId),
}

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TypeId::TopLevel(id) => format!("{:?}", id),
            TypeId::FormalArg(body_id, formal_arg_id) => format!("{:?} :: {:?}", body_id, formal_arg_id),
            TypeId::Var(body_id, var_id) => format!("{:?} :: {:?}", body_id, var_id),
            TypeId::Expr(body_id, expr_id) => format!("{:?} :: {:?}", body_id, expr_id),
        };
        write!(f, "{:?}", s)
    }
}

pub trait NumId: Clone + Copy + std::fmt::Debug + Eq + std::hash::Hash + Ord + PartialEq + PartialOrd {
    fn new(id: usize) -> Self;

    fn into_usize(self) -> usize;
}

pub trait DoubleNumId: Clone + Copy + std::fmt::Debug + Eq + std::hash::Hash + Ord + PartialEq + PartialOrd {
    fn new(first_id: usize, second_id: usize) -> Self;

    fn into_usize(self) -> (usize, usize);
}

pub trait TripleNumId: Clone + Copy + std::fmt::Debug + Eq + std::hash::Hash + Ord + PartialEq + PartialOrd {
    fn new(first_id: usize, second_id: usize, third_id: usize) -> Self;

    fn into_usize(self) -> (usize, usize, usize);
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct HakoId {
    id: usize,
}

impl NumId for HakoId {
    fn new(id: usize) -> Self {
        Self { id }
    }

    fn into_usize(self) -> usize {
        self.id
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct ModId {
    hako_id: usize,
    mod_id: usize,
}

impl DoubleNumId for ModId {
    fn new(hako_id: usize, mod_id: usize) -> Self {
        Self { hako_id, mod_id }
    }

    fn into_usize(self) -> (usize, usize) {
        (self.hako_id, self.mod_id)
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct ItemId {
    hako_id: usize,
    item_id: usize,
}

impl DoubleNumId for ItemId {
    fn new(hako_id: usize, item_id: usize) -> Self {
        Self { hako_id, item_id }
    }

    fn into_usize(self) -> (usize, usize) {
        (self.hako_id, self.item_id)
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct ItemMemberId {
    hako_id: usize,
    item_id: usize,
    member_id: usize,
}

impl TripleNumId for ItemMemberId {
    fn new(hako_id: usize, item_id: usize, member_id: usize) -> Self {
        Self { hako_id, item_id, member_id }
    }

    fn into_usize(self) -> (usize, usize, usize) {
        (self.hako_id, self.item_id, self.member_id)
    }
}

#[derive(Clone, Copy, std::fmt::Debug, Eq, std::hash::Hash, Ord, PartialEq, PartialOrd)]
pub struct BodyId {
    id: usize,
}

impl NumId for BodyId {
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
