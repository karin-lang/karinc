use super::{Local, LocalId};

pub struct BodyScopeHierarchy {
    scopes: Vec<BodyScope>,
}

impl BodyScopeHierarchy {
    pub fn new() -> BodyScopeHierarchy {
        BodyScopeHierarchy { scopes: Vec::new() }
    }

    pub fn get_current_scope(&self) -> &BodyScope {
        self.scopes.last().expect("could not get current body scope")
    }

    pub fn get_current_scope_mut(&mut self) -> &mut BodyScope {
        self.scopes.last_mut().expect("could not get current body scope")
    }

    pub fn get_current_locals(&self) -> &Vec<Local> {
        &self.get_current_scope().locals
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(BodyScope::new());
    }

    pub fn leave_scope(&mut self) -> Vec<Local> {
        self.scopes.pop().expect("could not leave body scope").exit()
    }

    pub fn declare(&mut self, id: &str, local: Local) -> LocalId {
        self.get_current_scope_mut().declare(id, local)
    }

    pub fn resolve(&self, id: &str) -> Option<LocalId> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(local_id) = each_scope.resolve(id) {
                return Some(local_id);
            }
        }

        None
    }
}

pub struct BodyScope {
    locals: Vec<Local>,
    scopes: Vec<LocalScope>,
}

impl BodyScope {
    pub fn new() -> BodyScope {
        BodyScope {
            locals: Vec::new(),
            scopes: vec![LocalScope::new()],
        }
    }

    pub fn exit(self) -> Vec<Local> {
        self.locals
    }

    pub fn get_current_scope_mut(&mut self) -> &mut LocalScope {
        self.scopes.last_mut().expect("could not get current local scope")
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(LocalScope::new());
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop().expect("could not leave local scope");
    }

    pub fn declare(&mut self, id: &str, local: Local) -> LocalId {
        let local_id = LocalId::from(self.locals.len());
        self.get_current_scope_mut().declare(id, local_id.clone());
        self.locals.push(local);
        local_id
    }

    pub fn resolve(&self, id: &str) -> Option<LocalId> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(local_id) = each_scope.resolve(id) {
                return Some(local_id);
            }
        }

        None
    }
}

pub struct LocalScope {
    id_decls: Vec<(String, LocalId)>,
}

impl LocalScope {
    pub fn new() -> LocalScope {
        LocalScope {
            id_decls: Vec::new(),
        }
    }

    pub fn declare(&mut self, id: &str, local_id: LocalId) {
        self.id_decls.push((id.to_string(), local_id));
    }

    pub fn resolve(&self, id: &str) -> Option<LocalId> {
        for (each_id, each_local_id) in self.id_decls.iter().rev() {
            if each_id == id {
                return Some(each_local_id.clone());
            }
        }

        None
    }
}
