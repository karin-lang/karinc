use std::collections::HashMap;

use crate::parser::ast;
use crate::hir::id::*;
use crate::typesys::{FnType, Type};

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
                    // item constraint
                    let ret_type = match &decl.body.ret_type {
                        Some(r#type) => r#type.into(),
                        None => Type::Prim(ast::PrimType::None),
                    };
                    let arg_types = decl.body.args.iter().map(|arg| (&arg.r#type).into()).collect();
                    let r#type = Type::Fn(FnType { ret_type: Box::new(ret_type.clone()), arg_types });
                    self.add_constraint(TopLevelId::Item(each_item.id), r#type);
                    // return type constraint
                    self.add_constraint(TopLevelId::FnRet(each_item.id), ret_type);
                    // argument constraint
                    for (i, each_arg) in decl.body.args.iter().enumerate() {
                        self.add_constraint(TopLevelId::FnArg(each_item.id, FormalArgId::new(i)), (&each_arg.r#type).into());
                    }
                },
            }
        }
    }

    pub fn get(&self, top_level_id: &TopLevelId) -> Option<&Type> {
        self.table.get(top_level_id)
    }

    pub fn get_fn(&self, item_id: &ItemId) -> Option<&FnType> {
        if let Some(r#type) = self.get(&TopLevelId::Item(*item_id)) {
            if let Type::Fn(fn_type) = r#type {
                return Some(fn_type)
            }
        }
        None
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
