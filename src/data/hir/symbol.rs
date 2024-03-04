pub mod entity;

use std::collections::HashMap;

pub trait HirCount: Copy {
    fn new(v: usize) -> Self;

    fn value(self) -> usize;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirPath {
    pub segments: Vec<String>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirMemberAccessChain {
    pub segments: Vec<String>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirGlobalSymbol {
    pub path: HirPath,
}

impl From<HirDividedGlobalSymbol> for HirGlobalSymbol {
    fn from(mut value: HirDividedGlobalSymbol) -> Self {
        let mut path = value.parent_module_path;
        path.segments.append(&mut value.following_path.segments);
        HirGlobalSymbol { path }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDividedGlobalSymbol {
    pub parent_module_path: HirPath,
    pub following_path: HirPath,
}

impl HirDividedGlobalSymbol {
    pub fn from_module_path(segments: Vec<String>) -> HirDividedGlobalSymbol {
        let mut parent = segments;
        let following = parent.pop().expect("expected path segments of at least one length");

        HirDividedGlobalSymbol {
            parent_module_path: HirPath { segments: parent },
            following_path: HirPath { segments: vec![following] },
        }
    }

    pub fn from_located_module_path_and_id(located_module_path: HirPath, id: String) -> HirDividedGlobalSymbol {
        let mut parent = located_module_path;
        let following = match parent.segments.pop() {
            Some(located) => vec![located, id],
            None => vec![id],
        };

        HirDividedGlobalSymbol {
            parent_module_path: parent,
            following_path: HirPath { segments: following },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirSymbolCodeOrPath {
    SymbolCode(HirSymbolCode),
    Path(HirPath),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct HirSymbolCode(usize);

impl HirCount for HirSymbolCode {
    fn new(v: usize) -> Self {
        Self(v)
    }

    fn value(self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirLocalSymbol {
    pub id: String,
    pub code: HirSymbolCode,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct HirSymbolIndex(usize);

impl HirSymbolIndex {
    pub fn new(v: usize) -> HirSymbolIndex {
        HirSymbolIndex(v)
    }
}

impl HirCount for HirSymbolIndex {
    fn new(v: usize) -> Self {
        Self(v)
    }

    fn value(self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirSymbolAccessorMap(HashMap<HirSymbolIndex, HirSymbolCodeOrPath>);

impl HirSymbolAccessorMap {
    pub fn new() -> HirSymbolAccessorMap {
        HirSymbolAccessorMap(HashMap::new())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirSymbolAccessor {
    pub index: HirSymbolIndex,
    pub kind: HirSymbolAccessorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirSymbolAccessorKind {
    SingleSegment(String),
    MultipleSegments(HirPath, HirMemberAccessChain),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirCounter<T: HirCount> {
    next: T,
}

impl<T: HirCount> HirCounter<T> {
    pub fn new() -> HirCounter<T> {
        HirCounter { next: T::new(0) }
    }

    pub fn generate(&mut self) -> T {
        let next = self.next;
        self.next = T::new(self.next.value() + 1);
        next
    }
}
