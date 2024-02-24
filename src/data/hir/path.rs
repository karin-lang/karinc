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

#[derive(Clone, Debug, PartialEq)]
pub struct HirRefIdOrPath {
    pub segments: Vec<String>,
    pub name_resolution_status: HirNameResolutionStatus,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirLocalCode(pub usize);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefLocalCode {
    pub id: HirDefId,
    pub code: HirLocalCode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirRefLocalCode {
    pub code: HirLocalCode,
    pub name_resolution_status: HirNameResolutionStatus,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirLocalCodeGenerator {
    next: usize,
}

impl HirLocalCodeGenerator {
    pub fn new() -> HirLocalCodeGenerator {
        HirLocalCodeGenerator { next: 0 }
    }

    pub fn generate(&mut self) -> HirLocalCode {
        let next = self.next;
        self.next += 1;
        HirLocalCode(next)
    }
}
