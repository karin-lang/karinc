use crate::hir::*;
use crate::hir::log::{HirLoweringLog, HirLoweringResult};
use crate::hir::resolve::*;

pub struct HirLowering<'a> {
    asts: &'a Vec<&'a ast::Ast>,
    current_mod_id: Option<ModId>,
    current_mod_path: ast::Path,
    paths: HashMap<ast::Path, GlobalId>,
    body_scope_hierarchy: BodyScopeHierarchy,
    logs: HashMap<ModId, Vec<HirLoweringLog>>,
}

impl<'a> HirLowering<'a> {
    pub fn new(asts: &'a Vec<&'a ast::Ast>) -> HirLowering<'a> {
        let mut lowering = HirLowering {
            asts,
            current_mod_id: None,
            current_mod_path: ast::Path::new(),
            paths: HashMap::new(),
            body_scope_hierarchy: BodyScopeHierarchy::new(),
            logs: HashMap::new(),
        };
        lowering.collect();
        lowering
    }

    // debug situation:
    // 1. Lowering module of ModId(0, 0).
    // 2. Hako name is "my_hako".
    // 3. Has specified path map.
    pub fn debug(&mut self, paths: HashMap<ast::Path, GlobalId>) {
        self.current_mod_id = Some(ModId::new(0, 0));
        self.current_mod_path = "my_hako".into();
        self.paths = paths;
    }

    // debug situation:
    // 1. situation in debug()
    // 2. Lowering in one layer body scope.
    pub fn debug_in_body(&mut self, paths: HashMap<ast::Path, GlobalId>) {
        self.debug(paths);
        self.body_scope_hierarchy.enter_scope();
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

    pub fn get_logs(&self) -> &HashMap<ModId, Vec<HirLoweringLog>> {
        &self.logs
    }

    pub fn collect_log<T>(&mut self, result: HirLoweringResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(e) => {
                let mod_id = self.current_mod_id.expect("current module id is not set.");
                match self.logs.get_mut(&mod_id) {
                    Some(v) => v.push(e),
                    None => {
                        let _ = self.logs.insert(mod_id, vec![e]);
                    },
                }
                None
            },
        }
    }

    pub fn get_body_scope_hierarchy(&self) -> &BodyScopeHierarchy {
        &self.body_scope_hierarchy
    }

    pub fn get_body_scope_hierarchy_mut(&mut self) -> &mut BodyScopeHierarchy {
        &mut self.body_scope_hierarchy
    }

    pub fn lower(mut self) -> (Hir, HashMap<ModId, Vec<HirLoweringLog>>) {
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
                    let log = HirLoweringLog::ExpectedExprButFoundHako { hako_id, span: span.clone() };
                    self.collect_log::<()>(Err(log));
                    return None;
                },
                GlobalId::Mod(mod_id) => {
                    let log = HirLoweringLog::ExpectedExprButFoundMod { mod_id, span: span.clone() };
                    self.collect_log::<()>(Err(log));
                    return None;
                },
                GlobalId::Item(item_id) => TopLevelId::Item(item_id),
                GlobalId::ItemMember(item_member_id) => TopLevelId::ItemMember(item_member_id),
            };
            return Some(
                Expr {
                    id: self.body_scope_hierarchy.generate_expr_id(),
                    kind: ExprKind::TopLevelRef(top_level_id, path),
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
        self.current_mod_id = Some(ast.mod_id);
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
        let mod_id = self.current_mod_id.expect("current module id is not set.");
        Item { id: item.id, mod_id, accessibility: item.accessibility.clone(), kind }
    }

    pub fn lower_fn_decl(&mut self, decl: &ast::FnDecl) -> FnDecl {
        let body = self.lower_body(&decl.body);
        let decl = FnDecl { body };
        decl
    }

    pub fn lower_body(&mut self, body: &ast::Body) -> Body {
        self.body_scope_hierarchy.enter_scope();
        body.args.iter().enumerate().for_each(|(i, arg)| self.lower_formal_arg(FormalArgId::new(i), arg));
        let id = body.id;
        let ret_type = body.ret_type.as_ref().map(|r#type| self.lower_type(r#type));
        let exprs = body.exprs.iter().map(|v| self.lower_expr(v)).collect();
        let (args, vars) = self.body_scope_hierarchy.leave_scope();
        Body { id, ret_type, args, vars, exprs }
    }

    pub fn lower_formal_arg(&mut self, id: FormalArgId, arg: &ast::FormalArg) {
        let arg_def = FormalArgDef {
            id,
            ref_mut: arg.ref_mut.clone(),
            r#type: self.lower_type(&arg.r#type),
        };
        self.body_scope_hierarchy.declare(&arg.id.id, LocalDef::FormalArg(arg_def));
    }

    pub fn lower_expr(&mut self, expr: &ast::Expr) -> Expr {
        // todo: 実装
        match &expr.kind {
            ast::ExprKind::Block(block) => Expr {
                id: self.body_scope_hierarchy.generate_expr_id(),
                kind: ExprKind::Block(self.lower_block(block)),
            },
            ast::ExprKind::Id(id) => {
                match self.resolve_id(&expr.span, &id.id) {
                    Some(v) => v,
                    None => {
                        self.collect_log::<()>(Err(HirLoweringLog::IdIsNotFoundInScope { id: id.clone(), span: expr.span.clone() }));
                        Expr { id: self.body_scope_hierarchy.generate_expr_id(), kind: ExprKind::Unknown }
                    },
                }
            },
            ast::ExprKind::Path(path) => {
                let top_level_id = match self.resolve_path(path) {
                    Some(global_id) => match global_id {
                        GlobalId::Hako(_) | GlobalId::Mod(_) => {
                            self.collect_log::<()>(Err(HirLoweringLog::UnnecessaryPath { path: path.clone(), span: expr.span.clone() }));
                            None
                        },
                        GlobalId::Item(item_id) => Some(TopLevelId::Item(item_id)),
                        GlobalId::ItemMember(item_member_id) => Some(TopLevelId::ItemMember(item_member_id)),
                    },
                    None => {
                        self.collect_log::<()>(Err(HirLoweringLog::PathIsNotFoundInScope { path: path.clone(), span: expr.span.clone() }));
                        None
                    },
                };
                let kind = match top_level_id {
                    Some(id) => ExprKind::TopLevelRef(id, path.clone()),
                    None => ExprKind::Unknown,
                };
                Expr { id: self.body_scope_hierarchy.generate_expr_id(), kind }
            },
            ast::ExprKind::Literal(literal) => {
                Expr { id: self.body_scope_hierarchy.generate_expr_id(), kind: ExprKind::Literal(literal.clone()) }
            },
            ast::ExprKind::Ret(ret) => {
                let new_expr_id = self.body_scope_hierarchy.generate_expr_id();
                let ret = self.lower_ret(ret);
                Expr { id: new_expr_id, kind: ExprKind::Ret(ret) }
            },
            ast::ExprKind::FnCall(call) => {
                let new_expr_id = self.body_scope_hierarchy.generate_expr_id();
                let call = self.lower_fn_call(call, expr.span.clone());
                Expr { id: new_expr_id, kind: ExprKind::FnCall(call) }
            },
            ast::ExprKind::VarDef(def) => self.lower_var_def(def),
            ast::ExprKind::VarBind(bind) => {
                let new_expr_id = self.body_scope_hierarchy.generate_expr_id();
                let hir_bind = self.lower_var_bind(bind);
                Expr {
                    id: new_expr_id,
                    kind: ExprKind::VarBind(hir_bind),
                }
            },
            ast::ExprKind::If(r#if) => Expr {
                id: self.body_scope_hierarchy.generate_expr_id(),
                kind: ExprKind::If(self.lower_if(r#if)),
            },
            ast::ExprKind::For(r#for) => Expr {
                id: self.body_scope_hierarchy.generate_expr_id(),
                kind: ExprKind::For(self.lower_for(r#for)),
            },
        }
    }

    pub fn lower_ret(&mut self, ret: &ast::Ret) -> Ret {
        let value = self.lower_expr(&ret.value);
        Ret { value: Box::new(value) }
    }

    pub fn lower_var_def(&mut self, def: &ast::VarDef) -> Expr {
        let new_expr_id = self.body_scope_hierarchy.generate_expr_id();
        let var_def = VarDef {
            id: def.id.clone(),
            ref_mut: def.ref_mut.clone(),
            r#type: def.r#type.as_ref().map(|r#type| self.lower_type(r#type)),
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
    }

    pub fn lower_var_bind(&mut self, bind: &ast::VarBind) -> VarBind {
        let var_id = self.resolve_var(&bind.id.id).unwrap(); // fix unwrap
        let value_expr = self.lower_expr(&bind.value);
        VarBind {
            var_id,
            value: Box::new(value_expr),
        }
    }

    pub fn lower_block(&mut self, block: &ast::Block) -> Block {
        let exprs = block.exprs.iter().map(|expr| self.lower_expr(expr)).collect();
        Block { exprs }
    }

    pub fn lower_fn_call(&mut self, call: &ast::FnCall, span: token::Span) -> FnCall {
        let r#fn = match self.resolve_path(&call.path) {
            Some(global_id) => match global_id {
                GlobalId::Item(item_id) => Some((item_id, call.path.clone())),
                _ => {
                    self.collect_log::<()>(Err(HirLoweringLog::GlobalIdIsNotFound { global_id, span }));
                    None
                },
            },
            None => {
                self.collect_log::<()>(Err(HirLoweringLog::PathIsNotFoundInScope { path: call.path.clone(), span }));
                None
            },
        };
        let args = call.args
            .iter()
            .map(|arg| {
                let expr = self.lower_expr(&arg.expr);
                ActualArg { expr }
            })
            .collect();
        let call = FnCall { r#fn, args };
        call
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

    pub fn lower_if(&mut self, r#if: &ast::If) -> If {
        let cond = Box::new(self.lower_expr(&r#if.cond));
        let block = self.lower_block(&r#if.block);
        let elifs = r#if.elifs.iter().map(|elif| self.lower_elif(elif)).collect();
        let r#else = r#if.r#else.as_ref().map(|block| self.lower_block(block));
        If { cond, block, elifs, r#else }
    }

    pub fn lower_elif(&mut self, elif: &ast::Elif) -> Elif {
        let cond = Box::new(self.lower_expr(&elif.cond));
        let block = self.lower_block(&elif.block);
        Elif { cond, block }
    }

    pub fn lower_for(&mut self, r#for: &ast::For) -> For {
        let kind = self.lower_for_kind(&r#for.kind);
        let block = self.lower_block(&r#for.block);
        For { kind, block }
    }

    pub fn lower_for_kind(&mut self, kind: &ast::ForKind) -> ForKind {
        match kind {
            ast::ForKind::Endless => ForKind::Endless,
            ast::ForKind::Cond { cond } => ForKind::Cond {
                cond: Box::new(self.lower_expr(&cond)),
            },
            ast::ForKind::Range { index, range } => ForKind::Range {
                index: Box::new(self.lower_expr(&index)),
                range: Box::new(self.lower_expr(&range)),
            },
        }
    }
}
