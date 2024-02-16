pub mod expr;
pub mod item;

use self::item::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Hir {
    pub items: Vec<HirItem>,
}
