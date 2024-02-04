#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: Vec<HirItem>,
}

impl Hir {
    pub fn new() -> Hir {
        Hir {
            items: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirItem {
    Function(HirFunction),
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirFunction {
    pub id: String,
}
