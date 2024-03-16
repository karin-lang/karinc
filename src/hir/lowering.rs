use std::collections::HashMap;

use crate::parser::ast;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {}

pub struct HirLowering {
    module_path: ast::GlobalSymbol,
    logs: Vec<HirLoweringLog>,
}

impl HirLowering {
    pub fn new(module_path: ast::GlobalSymbol) -> HirLowering {
        HirLowering {
            module_path,
            logs: Vec::new(),
        }
    }

    pub fn lower(mut self, ast: &ast::Ast) -> (Hir, Vec<HirLoweringLog>) {
        let mut global_entities = HashMap::new();

        for each_item in ast.global_symbol_table.iter() {
            let (id, entity) = self.lower_item(each_item);
            global_entities.insert(id, entity);
        }

        let hir = Hir { global_entities };
        (hir, self.logs)
    }

    pub fn lower_item(&mut self, item: &ast::Item) -> GlobalEntity {
        match &*item.kind {
            ast::ItemKind::FnDecl(decl) => {
                let (id, hir_decl) = self.lower_fn_decl(decl);
                (id, GlobalEntity::FnDecl(hir_decl))
            },
        }
    }

    pub fn lower_fn_decl(&mut self, decl: &ast::FnDecl) -> (GlobalEntityId, FnDecl) {
        self.enter_body_scope();

        let entity_id = self.module_path.clone().add_segment(decl.id.id.clone());
        let args = decl.args.iter().map(|v| self.lower_formal_arg(v)).collect();
        let body = decl.body.iter().map(|v| self.lower_expr(v)).collect();

        let entities = self.leave_body_scope();
        let decl = FnDecl { entities, args, body };
        (entity_id, decl)
    }

    pub fn lower_formal_arg(&mut self, arg: &ast::FormalArg) -> LocalEntityId {
        let arg_entity = FormalArg;
        let entity = LocalEntity::FormalArg(arg_entity);
        self.declare_id(arg.id.clone(), entity)
    }

    pub fn lower_expr(&mut self, _expr: &ast::Expr) -> Expr {
        // todo: 実装
        unimplemented!();
        // match &expr.kind {
        //     ast::ExprKind::VarDecl(_) => ,
        //     ast::ExprKind::VarInit(_) => ,
        // }
    }
}
