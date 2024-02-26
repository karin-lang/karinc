pub trait HirCount: Copy {
    fn new(v: usize) -> Self;

    fn value(self) -> usize;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirGlobalSymbol {
    pub segments: Vec<String>,
}

impl HirGlobalSymbol {
    pub fn new_with_parent(parent_segments: Vec<String>, last_segment: String) -> HirGlobalSymbol {
        let mut segments = parent_segments;
        segments.push(last_segment);
        HirGlobalSymbol { segments }
    }
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
pub struct HirSymbolAccessor {
    pub segments: Vec<String>,
    pub index: HirSymbolIndex,
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
