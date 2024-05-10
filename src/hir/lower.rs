use resolve::*;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    ExpectedExprFoundHako(token::Span, HakoId),
    ExpectedExprFoundMod(token::Span, ModId),
}

pub type HirLoweringResult<T> = Result<T, HirLoweringLog>;

pub struct HirLowering<'a> {
    asts: &'a Vec<&'a ast::Ast>,
    current_mod_path: ast::Path,
    paths: HashMap<ast::Path, GlobalId>,
    body_scope_hierarchy: BodyScopeHierarchy,
    logs: Vec<HirLoweringLog>,
}

impl<'a> HirLowering<'a> {
    pub fn new(asts: &'a Vec<&'a ast::Ast>) -> HirLowering<'a> {
        let mut lowering = HirLowering {
            asts,
            current_mod_path: ast::Path::new(),
            paths: HashMap::new(),
            body_scope_hierarchy: BodyScopeHierarchy::new(),
            logs: Vec::new(),
        };
        lowering.collect();
        lowering
    }

    pub fn collect(&mut self) {
        // todo: HakoIdとItemMemberIdを収集する
        for each_ast in self.asts {
            self.paths.insert(each_ast.mod_path.clone(), GlobalId::Mod(each_ast.mod_id));
            for each_item in &each_ast.items {
                let new_item_path = each_ast.mod_path.clone().add_segment(&each_item.name.id);
                self.paths.insert(new_item_path, GlobalId::Item(each_item.id));
            }
        }
    }

    pub fn collect_log<T>(&mut self, result: HirLoweringResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(e) => {
                self.logs.push(e);
                None
            },
        }
    }

    pub fn lower(mut self) -> (Hir, Vec<HirLoweringLog>) {
        let mut items = HashMap::new();
        for each_ast in self.asts {
            self.lower_ast(&mut items, each_ast);
        }
        let hir = Hir { items };
        (hir, self.logs)
    }

    pub fn resolve_id(&mut self, span: &token::Span, id: &str) -> Option<Expr> {
        if let Some(local_id) = self.resolve_local(id) {
            return Some(
                Expr {
                    id: self.body_scope_hierarchy.generate_expr_id(),
                    kind: ExprKind::LocalRef(local_id),
                },
            );
        }
        let path = self.get_item_path(id);
        if let Some(global_id) = self.resolve_path(&path) {
            // todo: test errors
            let top_level_id = match global_id {
                GlobalId::Hako(hako_id) => {
                    self.collect_log::<()>(Err(HirLoweringLog::ExpectedExprFoundHako(span.clone(), hako_id)));
                    return None;
                },
                GlobalId::Mod(mod_id) => {
                    self.collect_log::<()>(Err(HirLoweringLog::ExpectedExprFoundMod(span.clone(), mod_id)));
                    return None;
                },
                GlobalId::Item(item_id) => TopLevelId::Item(item_id),
                GlobalId::ItemMember(item_member_id) => TopLevelId::ItemMember(item_member_id),
            };
            return Some(
                Expr {
                    id: self.body_scope_hierarchy.generate_expr_id(),
                    kind: ExprKind::TopLevelRef(top_level_id),
                },
            );
        }
        None
    }

    pub fn resolve_var(&mut self, id: &str) -> Option<VarId> {
        self.body_scope_hierarchy.resolve_var(id)
    }

    pub fn resolve_path(&self, path: &ast::Path) -> Option<GlobalId> {
        if let Some(global_id) = self.paths.get(path) {
            return Some(*global_id);
        }

        let mut item_path = path.clone();
        while let Some(_) = item_path.pop_segment() {
            if let Some(global_id) = self.paths.get(&path) {
                return Some(*global_id);
            }
        }

        None
    }

    pub fn resolve_local(&self, id: &str) -> Option<LocalId> {
        self.body_scope_hierarchy.resolve(id)
    }

    pub fn get_item_path(&self, id: &str) -> ast::Path {
        self.current_mod_path.clone().add_segment(id)
    }

    pub fn lower_ast(&mut self, hir_items: &mut HashMap<ast::Path, Item>, ast: &ast::Ast) {
        self.current_mod_path = ast.mod_path.clone();
        for each_item in &ast.items {
            let new_hir_item = self.lower_item(each_item);
            let new_hir_path = self.get_item_path(&each_item.name.id);
            hir_items.insert(new_hir_path, new_hir_item);
        }
    }

    pub fn lower_item(&mut self, item: &ast::Item) -> Item {
        let kind = match &item.kind {
            ast::ItemKind::FnDecl(decl) => {
                let hir_decl = self.lower_fn_decl(decl);
                ItemKind::FnDecl(hir_decl)
            },
        };
        Item { id: item.id, kind }
    }

    pub fn lower_fn_decl(&mut self, decl: &ast::FnDecl) -> FnDecl {
        let body = self.lower_body(&decl.body);
        let decl = FnDecl { body };
        decl
    }

    pub fn lower_body(&mut self, body: &ast::Body) -> Body {
        self.body_scope_hierarchy.enter_scope();
        body.args.iter().for_each(|v| self.lower_formal_arg(v));
        let ret_type = body.ret_type.as_ref().map(|r#type| self.lower_type(r#type));
        let exprs = body.exprs.iter().map(|v| self.lower_expr(v)).collect();
        let (args, vars) = self.body_scope_hierarchy.leave_scope();
        Body { ret_type, args, vars, exprs }
    }

    pub fn lower_formal_arg(&mut self, arg: &ast::FormalArg) {
        let arg_def = FormalArgDef {
            r#type: self.lower_type(&arg.r#type),
            mutable: arg.mutable,
        };
        self.body_scope_hierarchy.declare(&arg.id.id, LocalDef::FormalArg(arg_def));
    }

    pub fn lower_expr(&mut self, expr: &ast::Expr) -> Expr {
        // todo: 実装
        match &expr.kind {
            ast::ExprKind::Id(id) => self.resolve_id(&expr.span, &id.id).unwrap(), //fix unwrap()
            ast::ExprKind::VarDef(def) => {
                let new_expr_id = self.body_scope_hierarchy.generate_expr_id();
                let var_def = VarDef {
                    r#type: def.r#type.as_ref().map(|r#type| self.lower_type(r#type)),
                    mutable: false,
                    init: def.init.as_ref().map(|expr| self.lower_expr(expr)),
                };
                let local_id = self.body_scope_hierarchy.declare(&def.id.id, LocalDef::Var(var_def));
                match local_id {
                    LocalId::Var(var_id) => Expr {
                        id: new_expr_id,
                        kind: ExprKind::VarDef(var_id),
                    },
                    _ => unreachable!(),
                }
            },
            ast::ExprKind::VarBind(bind) => {
                let var_id = self.resolve_var(&bind.id.id).unwrap(); // fix unwrap
                let value_expr = self.lower_expr(&bind.value);
                Expr {
                    id: self.body_scope_hierarchy.generate_expr_id(),
                    kind: ExprKind::VarBind(
                        VarBind {
                            var_id,
                            value: Box::new(value_expr),
                        },
                    ),
                }
            },
            _ => unimplemented!(),
        }
    }

    pub fn lower_type(&mut self, r#type: &ast::Type) -> Type {
        let kind = match &*r#type.kind {
            ast::TypeKind::Id(id) => {
                let path = self.get_item_path(&id.id);
                match self.resolve_path(&path) {
                    Some(global_id) => match global_id {
                        GlobalId::Item(item_id) => TypeKind::Item(item_id),
                        _ => unimplemented!(),
                    },
                    None => unimplemented!(),
                }
            },
            ast::TypeKind::Prim(prim_type) => TypeKind::Prim(*prim_type),
        };
        Type::new(kind)
    }
}
