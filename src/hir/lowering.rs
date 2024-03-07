use std::collections::HashMap;

use crate::parser::ast;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct LocalScopeHierarchy {
    scopes: Vec<LocalScope>,
    entities: HashMap<LocalEntityId, LocalEntity>,
    entity_id_counter: usize,
}

impl LocalScopeHierarchy {
    pub fn new() -> LocalScopeHierarchy {
        LocalScopeHierarchy {
            scopes: Vec::new(),
            entities: HashMap::new(),
            entity_id_counter: 0,
        }
    }

    pub fn get_current_scope(&mut self) -> &mut LocalScope {
        self.scopes.last_mut().expect("could not get current scope")
    }

    pub fn enter_named_scope(&mut self, id: Id, entity_id: LocalEntityId) {
        let mut scope = LocalScope::new(Some(entity_id.clone()));
        scope.declare_id(id, entity_id);
        self.scopes.push(scope);
    }

    pub fn enter_unnamed_scope(&mut self) {
        let scope = LocalScope::new(None);
        self.scopes.push(scope);
    }

    // note: スコープ内から呼び出してください。
    pub fn leave_scope(&mut self) {
        self.scopes.pop().expect("could not leave current scope");
    }

    pub fn generate_entity_id(&mut self) -> LocalEntityId {
        let id = self.entity_id_counter.into();
        self.entity_id_counter += 1;
        id
    }

    pub fn declare_id(&mut self, id: Id, entity: LocalEntity) -> LocalEntityId {
        let entity_id = self.generate_entity_id();
        self.get_current_scope().declare_id(id, entity_id.clone());
        self.entities.insert(entity_id.clone(), entity);
        entity_id
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalScope {
    id: Option<LocalEntityId>,
    declared_ids: Vec<(Id, LocalEntityId)>,
}

impl LocalScope {
    pub fn new(id: Option<LocalEntityId>) -> LocalScope {
        LocalScope {
            id,
            declared_ids: Vec::new(),
        }
    }

    pub fn declare_id(&mut self, id: Id, entity_id: LocalEntityId) {
        self.declared_ids.push((id, entity_id));
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HirLoweringLog {
    UnknownNodeId(String),
}

pub struct HirLowering {
    module_path: GlobalEntityId,
    local_scope_hierarchies: Vec<LocalScopeHierarchy>,
    logs: Vec<HirLoweringLog>,
}

impl HirLowering {
    pub fn new(module_path: GlobalEntityId) -> HirLowering {
        HirLowering {
            module_path,
            local_scope_hierarchies: Vec::new(),
            logs: Vec::new(),
        }
    }

    pub fn enter_body_scope(&mut self) {
        let scope_hierarchy = LocalScopeHierarchy::new();
        self.local_scope_hierarchies.push(scope_hierarchy);
    }

    // note: ボディスコープ内から呼び出してください。
    pub fn leave_body_scope(&mut self) -> HashMap<LocalEntityId, LocalEntity> {
        let hierarchy = self.local_scope_hierarchies.pop().expect("could not leave body scope");
        hierarchy.entities
    }

    // note: ボディスコープ内から呼び出してください。
    pub fn get_current_local_hierarchy(&mut self) -> &mut LocalScopeHierarchy {
        self.local_scope_hierarchies.last_mut().expect("could not get current local hierarchy")
    }

    // note: ボディスコープ内から呼び出してください。
    pub fn declare_id(&mut self, id: Id, entity: LocalEntity) -> LocalEntityId {
        let hierarchy = self.get_current_local_hierarchy();
        hierarchy.declare_id(id, entity)
    }

    pub fn lower(mut self, ast: &ast::Ast) -> (Hir, Vec<HirLoweringLog>) {
        let mut global_entities = HashMap::new();

        for each_item in &ast.items {
            let (id, entity) = self.lower_item(each_item);
            global_entities.insert(id, entity);
        }

        let hir = Hir { global_entities };
        (hir, self.logs)
    }

    pub fn lower_item(&mut self, item: &ast::Item) -> (GlobalEntityId, GlobalEntity) {
        match &item.kind {
            ast::ItemKind::FnDecl(decl) => {
                let (id, hir_decl) = self.lower_fn_decl(decl);
                (id, GlobalEntity::FnDecl(hir_decl))
            },
        }
    }

    pub fn lower_fn_decl(&mut self, decl: &ast::FnDecl) -> (GlobalEntityId, FnDecl) {
        self.enter_body_scope();

        let entity_id = self.module_path.clone().add_segment(decl.id.clone());
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
