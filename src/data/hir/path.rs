#[derive(Clone, Debug, PartialEq)]
pub struct HirNameResolutionTarget {
    pub segments: Vec<String>,
    pub name_resolution_status: HirNameResolutionStatus,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirNameResolutionStatus {
    ResolvedAsPath(HirDefPath),
    ResolvedAsLocalCode(HirLocalCode),
    NotFoundInScope,
    Unresolved,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefId(pub String);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefPath(pub Vec<String>);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefLocalCode(pub HirLocalCode);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirLocalCode(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub struct HirCodeGenerator {
    next: usize,
}

impl HirCodeGenerator {
    pub fn new() -> HirCodeGenerator {
        HirCodeGenerator { next: 0 }
    }

    pub fn generate(&mut self) -> HirLocalCode {
        let next = self.next;
        self.next += 1;
        HirLocalCode(next)
    }
}
