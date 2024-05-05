use std::collections::{HashSet, VecDeque};

use resolve::*;
use self::ast::Path;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {}

pub struct HirLowering<'a> {
    asts: &'a Vec<ast::Ast>,
    current_mod_path: ast::Path,
    paths: HashSet<ast::Path>,
    body_scope_hierarchy: BodyScopeHierarchy,
    logs: Vec<HirLoweringLog>,
}

impl<'a> HirLowering<'a> {
    pub fn new(asts: &'a Vec<ast::Ast>) -> HirLowering<'a> {
        let mut lowering = HirLowering {
            asts,
            current_mod_path: ast::Path::new(),
            paths: HashSet::new(),
            body_scope_hierarchy: BodyScopeHierarchy::new(),
            logs: Vec::new(),
        };
        lowering.collect();
        lowering
    }

    pub fn collect(&mut self) {
        for each_ast in self.asts {
            self.paths.insert(each_ast.mod_path.clone());
            for each_item in &each_ast.items {
                let new_item_path = each_ast.mod_path.clone().add_segment(&each_item.id.id);
                self.paths.insert(new_item_path);
            }
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

    pub fn resolve_id(&mut self, id: &str) -> Option<Expr> {
        if let Some(local_id) = self.resolve_local(id) {
            return Some(
                Expr {
                    id: self.body_scope_hierarchy.generate_expr_id(),
                    kind: ExprKind::LocalRef(local_id),
                },
            );
        }
        let path = self.get_item_path(id);
        if let Some(expr) = self.resolve_path(&path) {
            return Some(
                Expr {
                    id: self.body_scope_hierarchy.generate_expr_id(),
                    kind: ExprKind::PathRef(expr),
                },
            );
        }
        None
    }

    pub fn resolve_var(&mut self, id: &str) -> Option<VarId> {
        self.body_scope_hierarchy.resolve_var(id)
    }

    pub fn resolve_path(&self, path: &ast::Path) -> Option<DivPath> {
        if self.paths.contains(path) {
            let div_path = DivPath {
                item_path: path.clone(),
                following_path: Path::new(),
            };
            return Some(div_path);
        }

        let mut item_path = path.clone();
        let mut following_segments = VecDeque::new();
        while let Some(each_segment) = item_path.pop_segment() {
            if self.paths.contains(&path) {
                let div_path = DivPath {
                    item_path,
                    following_path: Path::from(following_segments.into()),
                };
                return Some(div_path);
            }
            following_segments.push_front(each_segment);
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
            let new_hir_path = self.get_item_path(&each_item.id.id);
            hir_items.insert(new_hir_path, new_hir_item);
        }
    }

    pub fn lower_item(&mut self, item: &ast::Item) -> Item {
        match &item.kind {
            ast::ItemKind::FnDecl(decl) => {
                let hir_decl = self.lower_fn_decl(decl);
                Item::FnDecl(hir_decl)
            },
        }
    }

    pub fn lower_fn_decl(&mut self, decl: &ast::FnDecl) -> FnDecl {
        let body = self.lower_body(&decl.body);
        let decl = FnDecl { body };
        decl
    }

    pub fn lower_body(&mut self, body: &ast::Body) -> Body {
        self.body_scope_hierarchy.enter_scope();
        body.args.iter().for_each(|v| self.lower_formal_arg(v));
        let exprs = body.exprs.iter().map(|v| self.lower_expr(v)).collect();
        let (args, vars) = self.body_scope_hierarchy.leave_scope();
        Body { args, vars, exprs }
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
            ast::ExprKind::Id(id) => self.resolve_id(&id.id).unwrap(), //fix unwrap()
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
                let div_path = self.resolve_path(&path);
                match div_path {
                    Some(v) => TypeKind::Path(v),
                    None => unimplemented!(),
                }
            },
            ast::TypeKind::Prim(prim_type) => TypeKind::Prim(*prim_type),
        };
        Type::new(kind)
    }
}
