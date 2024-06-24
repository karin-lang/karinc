use super::*;

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

    pub fn enter_scope(&mut self) {
        self.scopes.push(BodyScope::new());
    }

    pub fn leave_scope(&mut self) -> (Vec<FormalArgDef>, Vec<VarDef>) {
        self.scopes.pop().expect("could not leave body scope").exit()
    }

    pub fn declare(&mut self, id: &str, local_def: LocalDef) -> LocalId {
        self.get_current_scope_mut().declare(id, local_def)
    }

    pub fn resolve(&self, id: &str) -> Option<LocalId> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(id) = each_scope.resolve(id) {
                return Some(id);
            }
        }
        None
    }

    pub fn resolve_arg(&self, id: &str) -> Option<FormalArgId> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(id) = each_scope.resolve_arg(id) {
                return Some(id);
            }
        }
        None
    }

    pub fn resolve_var(&self, id: &str) -> Option<VarId> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(id) = each_scope.resolve_var(id) {
                return Some(id);
            }
        }
        None
    }

    pub fn generate_expr_id(&mut self) -> ExprId {
        self.get_current_scope_mut().generate_expr_id()
    }
}

pub struct BodyScope {
    args: Vec<FormalArgDef>,
    arg_ids: HashMap<String, FormalArgId>,
    vars: Vec<VarDef>,
    scopes: Vec<LocalScope>,
    next_expr_id: usize,
}

impl BodyScope {
    pub fn new() -> BodyScope {
        BodyScope {
            args: Vec::new(),
            arg_ids: HashMap::new(),
            vars: Vec::new(),
            scopes: vec![LocalScope::new()],
            next_expr_id: 0,
        }
    }

    pub fn exit(self) -> (Vec<FormalArgDef>, Vec<VarDef>) {
        (self.args, self.vars)
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

    pub fn declare(&mut self, id: &str, local_def: LocalDef) -> LocalId {
        match local_def {
            LocalDef::FormalArg(arg) => {
                let arg_id = FormalArgId::new(self.args.len());
                self.args.push(arg);
                self.arg_ids.insert(id.to_string(), arg_id);
                LocalId::FormalArg(arg_id)
            },
            LocalDef::Var(var) => {
                let var_id = VarId::new(self.vars.len());
                self.get_current_scope_mut().declare_var(id, var_id);
                self.vars.push(var);
                LocalId::Var(var_id)
            },
        }
    }

    pub fn resolve(&self, id: &str) -> Option<LocalId> {
        if let Some(id) = self.resolve_arg(id) {
            Some(LocalId::FormalArg(id))
        } else if let Some(id) = self.resolve_var(id) {
            Some(LocalId::Var(id))
        } else {
            None
        }
    }

    pub fn resolve_arg(&self, id: &str) -> Option<FormalArgId> {
        self.arg_ids.get(id).cloned()
    }

    pub fn resolve_var(&self, id: &str) -> Option<VarId> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(var_id) = each_scope.resolve_var(id) {
                return Some(var_id);
            }
        }
        None
    }

    pub fn generate_expr_id(&mut self) -> ExprId {
        let next = ExprId::new(self.next_expr_id);
        self.next_expr_id += 1;
        next
    }

    pub fn get_args(&self) -> &Vec<FormalArgDef> {
        &self.args
    }

    pub fn get_vars(&self) -> &Vec<VarDef> {
        &self.vars
    }
}

pub struct LocalScope {
    vars: Vec<(String, VarId)>,
}

impl LocalScope {
    pub fn new() -> LocalScope {
        LocalScope {
            vars: Vec::new(),
        }
    }

    pub fn declare_var(&mut self, id: &str, var_id: VarId) {
        self.vars.push((id.to_string(), var_id));
    }

    pub fn resolve_var(&self, id: &str) -> Option<VarId> {
        for (each_id, each_var_id) in self.vars.iter().rev() {
            if each_id == id {
                return Some(*each_var_id);
            }
        }
        None
    }
}
