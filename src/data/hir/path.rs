#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefId(pub String);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HirDefPath(pub Vec<String>);

#[derive(Clone, Debug, PartialEq)]
pub struct HirRefPath {
    pub segments: Vec<String>,
    pub name_resolution_status: HirNameResolutionStatus,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirNameResolutionStatus {
    Resolved(HirDefPath), // todo: パスと局所変数の両方に対応する
    NotFoundInScope,
    Unresolved,
}

// todo: ローカル識別子への利用を検討する
#[derive(Clone, Debug, PartialEq)]
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
