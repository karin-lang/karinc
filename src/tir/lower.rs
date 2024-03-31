use std::collections::HashMap;

use crate::hir;
use crate::tir::*;

pub struct TirLowering<'a> {
    hir: &'a hir::Hir,
}

impl<'a> TirLowering<'a> {
    pub fn new(hir: &'a hir::Hir) -> TirLowering<'a> {
        TirLowering { hir }
    }

    pub fn lower(mut self) -> Tir {
        let mut bodies = HashMap::new();
        for (each_item_path, each_item) in &self.hir.items {
            match each_item {
                hir::Item::FnDecl(decl) => {
                    bodies.insert(each_item_path.clone(), self.lower_fn_decl(decl));
                },
            }
        }
        Tir { bodies }
    }

    pub fn lower_fn_decl(&mut self, decl: &hir::FnDecl) -> Body {
        let mut locals = Vec::new();
        for hir_local in &decl.body.locals {
            let new_local = self.lower_local(hir_local);
            locals.push(new_local);
        }

        let mut exprs = Vec::new();
        for hir_expr in &decl.body.exprs {
            let new_expr = self.lower_expr(hir_expr);
            exprs.push(new_expr);
        }

        Body { locals, exprs }
    }

    pub fn lower_local(&mut self, local: &hir::Local) -> Local {
        match local {
            hir::Local::FormalArg(_) => unimplemented!(),
            hir::Local::VarDecl(_) => Local::VarDecl(
                VarDecl { r#type: Type::new(TypeKind::Infer) },
            ),
            hir::Local::VarInit(init) => Local::VarInit(
                VarInit { init: self.lower_expr(&init.init) }
            ),
        }
    }

    pub fn lower_expr(&mut self, expr: &hir::Expr) -> Expr {
        match expr {
            hir::Expr::FnCall(_) => unimplemented!(),
            hir::Expr::PathRef(path) => Expr { kind: ExprKind::PathRef(self.get_type_from_path(path)) },
            hir::Expr::LocalDecl(local_id) => Expr { kind: ExprKind::LocalDecl(local_id.clone()) },
            hir::Expr::LocalRef(local_id) => Expr { kind: ExprKind::LocalRef(local_id.clone()) },
        }
    }

    pub fn get_type_from_path(&self, path: &hir::DivPath) -> Type {
        // todo: path.following_path を考慮する
        let kind = match self.hir.items.get(&path.item_path) {
            Some(item) => if path.following_path.is_empty() {
                TypeKind::Item(path.item_path.clone())
            } else {
                match item {
                    // todo: 関数パスに following_path は付かないのでエラー
                    hir::Item::FnDecl(_) => unimplemented!(),
                }
            },
            None => TypeKind::Undefined,
        };
        Type::new(kind)
    }
}
