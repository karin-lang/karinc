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
            let new_local = self.lower_local(&decl.body.locals, hir_local);
            locals.push(new_local);
        }

        let mut exprs = Vec::new();
        for hir_expr in &decl.body.exprs {
            let new_expr = self.lower_expr(&decl.body.locals, hir_expr);
            exprs.push(new_expr);
        }

        Body { locals, exprs }
    }

    pub fn lower_local(&mut self, locals: &Vec<hir::Local>, local: &hir::Local) -> Local {
        match local {
            hir::Local::FormalArg(arg) => Local::FormalArg(
                FormalArg { r#type: self.lower_type(&arg.r#type) }
            ),
            hir::Local::VarDecl(decl) => Local::VarDecl(
                VarDecl {
                    r#type: match &decl.r#type {
                        Some(r#type) => self.lower_type(r#type),
                        None => Type::new(TypeKind::Infer),
                    },
                },
            ),
            hir::Local::VarInit(init) => Local::VarInit(
                VarInit {
                    r#type: match &init.r#type {
                        Some(r#type) => self.lower_type(r#type),
                        None => Type::new(TypeKind::Infer),
                    },
                    init: self.lower_expr(locals, &init.init),
                }
            ),
        }
    }

    pub fn lower_expr(&mut self, locals: &Vec<hir::Local>, expr: &hir::Expr) -> Expr {
        match expr {
            hir::Expr::FnCall(hir_call) => {
                let mut hir_formal_args = match self.hir.items.get(&hir_call.r#fn) {
                    Some(hir_item) => match hir_item {
                        hir::Item::FnDecl(decl) => decl.args.iter(),
                        _ => unimplemented!(),
                    },
                    None => unimplemented!(),
                };
                let mut actual_args = Vec::new();
                for each_hir_actual_arg in &hir_call.args {
                    let new_arg = ActualArg { expr: self.lower_expr(&each_hir_actual_arg.expr) };
                    if let Some(local_id) = hir_formal_args.next() {
                        if let Some(local) = locals.get(local_id.into()) {
                            match local {
                                hir::Local::FormalArg(_) => (),
                                hir::Local::VarDecl(decl) => ,
                                hir::Local::VarInit(init) => ,
                            }
                        }
                    };
                    actual_args.push(new_arg);
                }
                let call = FnCall { r#fn: hir_call.r#fn.clone(), args: actual_args };
                Expr { kind: ExprKind::FnCall(call) }
            },
            hir::Expr::PathRef(path) => Expr { kind: ExprKind::PathRef(self.get_type_from_path(path)) },
            hir::Expr::LocalDecl(local_id) => Expr { kind: ExprKind::LocalDecl(local_id.clone()) },
            hir::Expr::LocalRef(local_id) => Expr { kind: ExprKind::LocalRef(local_id.clone()) },
        }
    }

    pub fn lower_type(&mut self, r#type: &hir::Type) -> Type {
        match &*r#type.kind {
            hir::TypeKind::Path(path) => self.get_type_from_path(path),
            hir::TypeKind::Prim(prim_type) => Type::new(TypeKind::Prim(*prim_type)),
        }
    }

    pub fn get_type_from_path(&self, path: &hir::DivPath) -> Type {
        // todo: path.following_path を考慮する
        let kind = match self.hir.items.get(&path.item_path) {
            Some(item) => if path.is_item() {
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
