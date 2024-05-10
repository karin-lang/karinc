use std::collections::HashMap;

use crate::parser::ast;
use crate::hir::id::*;
use crate::tir::constraint::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct TopLevelTypeTable {
    table: HashMap<TopLevelId, Type>,
}

impl TopLevelTypeTable {
    pub fn new() -> TopLevelTypeTable {
        TopLevelTypeTable { table: HashMap::new() }
    }

    pub fn absorb(&mut self, ast: &ast::Ast) {
        for each_item in &ast.items {
            match &each_item.kind {
                ast::ItemKind::FnDecl(decl) => {
                    let ret_type = match &decl.body.ret_type {
                        Some(r#type) => r#type.into(),
                        None => Type::Void,
                    };
                    let arg_types = decl.body.args.iter().map(|arg| (&arg.r#type).into()).collect();
                    let r#type = Type::Fn { ret_type: Box::new(ret_type), arg_types };
                    self.add_constraint(TopLevelId::Item(each_item.id), r#type);
                },
            }
        }
    }

    pub fn get(&self, top_level_id: &TopLevelId) -> Option<&Type> {
        self.table.get(top_level_id)
    }

    pub fn add_constraint(&mut self, top_level_id: TopLevelId, r#type: Type) {
        self.table.insert(top_level_id, r#type);
    }
}

impl From<HashMap<TopLevelId, Type>> for TopLevelTypeTable {
    fn from(value: HashMap<TopLevelId, Type>) -> Self {
        Self { table: value }
    }
}
