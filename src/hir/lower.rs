use std::collections::HashMap;

use crate::parser::ast::{self, NodeId};
use resolve::*;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {}

pub struct HirLowering<'a> {
    ast: &'a ast::Ast,
    current_mod_path: Vec<ast::NodeId>,
    item_id_gen: ItemIdGen,
    item_id_map: HashMap<ast::NodeId, (String, ItemId)>,
    body_scope_hierarchy: BodyScopeHierarchy,
    logs: Vec<HirLoweringLog>,
}

impl<'a> HirLowering<'a> {
    pub fn new(ast: &'a ast::Ast) -> HirLowering<'a> {
        let mut lowering = HirLowering {
            ast,
            current_mod_path: Vec::new(),
            item_id_gen: ItemIdGen::new(),
            item_id_map: HashMap::new(),
            body_scope_hierarchy: BodyScopeHierarchy::new(),
            logs: Vec::new(),
        };
        lowering.collect();
        lowering
    }

    pub fn collect(&mut self) {
        for each_hako_mod_id in &self.ast.hako_mods {
            self.collect_mod(each_hako_mod_id);
        }
    }

    pub fn collect_mod(&mut self, mod_id: &ast::NodeId) {
        let mod_item = self.ast.items.get::<usize>(mod_id.clone().into()).unwrap();
        let r#mod = match &mod_item.kind {
            ast::ItemKind::Mod(r#mod) => r#mod,
            _ => unimplemented!(),
        };
        self.item_id_map.insert(mod_item.node_id.clone(), (mod_item.id.id.clone(), self.item_id_gen.generate()));
        for (each_item_id, each_item_node_id) in &r#mod.items {
            self.item_id_map.insert(each_item_node_id.clone(), (each_item_id.clone(), self.item_id_gen.generate()));
        }
        for each_submod_id in &r#mod.submods {
            self.collect_mod(each_submod_id);
        }
    }

    pub fn lower(mut self) -> (Hir, Vec<HirLoweringLog>) {
        let mut items = Vec::new();
        for each_mod_id in &self.ast.hako_mods {
            self.lower_mod(&mut items, each_mod_id);
        }
        let hir = Hir { items };
        (hir, self.logs)
    }

    pub fn resolve(&mut self, id: &str) -> Option<Expr> {
        if let Some(local_id) = self.resolve_local(id) {
            return Some(Expr::LocalRef(local_id));
        }
        if let Some(item_id) = self.resolve_item(id) {
            return Some(Expr::ItemRef(item_id));
        }
        None
    }

    pub fn resolve_item(&self, id: &str) -> Option<ItemId> {
        if let Some(parent_item) = self.ast.items.get::<usize>(self.current_mod_path.last().unwrap().clone().into()) {
            match &parent_item.kind {
                ast::ItemKind::Mod(r#mod) => {
                    for (each_id, each_node_id) in &r#mod.items {
                        if *each_id == id {
                            let (_, item_id) = self.item_id_map.get(each_node_id).unwrap();
                            return Some(item_id.clone());
                        }
                    }
                },
                _ => (),
            }
        }
        None
    }

    pub fn resolve_local(&self, id: &str) -> Option<LocalId> {
        self.body_scope_hierarchy.resolve(id)
    }

    pub fn lower_mod(&mut self, hir_items: &mut Vec<Item>, mod_id: &NodeId) {
        let mod_item = self.ast.items.get::<usize>(mod_id.clone().into()).unwrap();
        let r#mod = match &mod_item.kind {
            ast::ItemKind::Mod(r#mod) => r#mod,
            _ => unimplemented!(),
        };

        let hir_mod_item = self.lower_item(mod_item);
        hir_items.push(hir_mod_item);

        self.current_mod_path.push(mod_id.clone());
        for (_, each_item_node_id) in &r#mod.items {
            match self.ast.items.get::<usize>(each_item_node_id.clone().into()) {
                Some(each_item) => {
                    let new_hir_item = self.lower_item(each_item);
                    hir_items.push(new_hir_item);
                },
                None => unimplemented!(),
            }
        }

        for each_submod_id in &r#mod.submods {
            self.lower_mod(hir_items, each_submod_id);
        }

        self.current_mod_path.pop().unwrap();
    }

    pub fn lower_item(&mut self, item: &ast::Item) -> Item {
        match &item.kind {
            ast::ItemKind::Mod(_) => Item::Mod,
            ast::ItemKind::FnDecl(decl) => {
                let hir_decl = self.lower_fn_decl(decl);
                Item::FnDecl(hir_decl)
            },
        }
    }

    pub fn lower_fn_decl(&mut self, decl: &ast::FnDecl) -> FnDecl {
        let args = decl.args.iter().map(|v| self.lower_formal_arg(v)).collect();
        let body = self.lower_body(&decl.body);
        let decl = FnDecl { args, body };
        decl
    }

    pub fn lower_body(&mut self, body: &ast::Body) -> Body {
        self.body_scope_hierarchy.enter_scope();
        let exprs = body.exprs.iter().map(|v| self.lower_expr(v)).collect();
        let locals = self.body_scope_hierarchy.leave_scope();
        Body { exprs, locals }
    }

    pub fn lower_formal_arg(&mut self, arg: &ast::FormalArg) -> LocalId {
        let arg_entity = FormalArg;
        let local = Local::FormalArg(arg_entity);
        self.body_scope_hierarchy.declare(&arg.id.id, local)
    }

    pub fn lower_expr(&mut self, expr: &ast::Expr) -> Expr {
        // todo: 実装
        match &expr.kind {
            ast::ExprKind::Id(id) => self.resolve(&id.id).unwrap(), //fix unwrap()
            ast::ExprKind::VarDecl(decl) => {
                let local = Local::VarDecl(VarDecl { mutable: false }); // fix mutability
                let local_id = self.body_scope_hierarchy.declare(&decl.id.id, local);
                Expr::LocalDecl(local_id)
            },
            _ => unimplemented!(),
        }
    }
}
