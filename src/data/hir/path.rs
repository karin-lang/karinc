#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefId(pub String);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefPath(pub Vec<String>);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirSymbolCode(pub usize);

// note: 参照用シンボル
#[derive(Clone, Debug, PartialEq)]
pub struct HirSymbol {
    pub segments: Vec<String>,
    pub code: HirSymbolCode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirSymbolCodeGenerator {
    next: usize,
}

impl HirSymbolCodeGenerator {
    pub fn new() -> HirSymbolCodeGenerator {
        HirSymbolCodeGenerator { next: 0 }
    }

    pub fn generate(&mut self) -> HirSymbolCode {
        let next = self.next;
        self.next += 1;
        HirSymbolCode(next)
    }
}
