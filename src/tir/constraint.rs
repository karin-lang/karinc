use std::collections::HashMap;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

use crate::lexer::token;
use crate::parser::ast;
use crate::{hir, hir::id::*};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeLog {
    InconsistentConstraint,
}

pub type TypeResult<T> = Result<T, TypeLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Item(ast::Path),
    Prim(ast::PrimType),
    Undefined,
    Unresolved,
}

impl Type {
    pub fn is_resolved(&self) -> bool {
        *self != Type::Unresolved
    }
}

impl From<&hir::Type> for Type {
    fn from(value: &hir::Type) -> Self {
        match &*value.kind {
            hir::TypeKind::Path(_) => unimplemented!(),
            hir::TypeKind::Prim(prim_type) => Type::Prim(prim_type.clone()),
        }
    }
}

#[derive(PartialEq)]
pub struct TypePtr(Rc<RefCell<Type>>);

impl TypePtr {
    pub fn new(r#type: Type) -> TypePtr {
        TypePtr(Rc::new(RefCell::new(r#type)))
    }

    pub fn borrow(&self) -> Ref<Type> {
        (*self.0).borrow()
    }
}

impl Clone for TypePtr {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl std::fmt::Debug for TypePtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", *self.borrow())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstraintTable {
    table: HashMap<ExprId, TypeConstraint>,
}

impl TypeConstraintTable {
    pub fn new() -> TypeConstraintTable {
        TypeConstraintTable { table: HashMap::new() }
    }

    pub fn constrain(&mut self, expr_id: ExprId) {
        if !self.table.contains_key(&expr_id) {
            self.constrain_independent(expr_id, Type::Unresolved)
        }
    }

    pub fn constrain_independent(&mut self, expr_id: ExprId, r#type: Type) {
        let constraint = TypeConstraint::Independent { ptr: TypePtr::new(r#type) };
        self.table.insert(expr_id, constraint);
    }

    pub fn constrain_dependent(&mut self, expr_id: ExprId, constrained_by: ExprId) -> TypeResult<()> {
        let new_ptr = match self.table.get(&constrained_by) {
            Some(constraint) => constraint.get_ptr().clone(),
            None => panic!("unknown expression id"),
        };
        if let Some(constraint) = self.table.get_mut(&expr_id) {
            let ptr = constraint.get_ptr().borrow();
            if ptr.is_resolved() && *ptr != *new_ptr.borrow() {
                return Err(TypeLog::InconsistentConstraint);
            }
        }
        let new_constraint = TypeConstraint::Dependent { ptr: new_ptr, constrained_by };
        self.table.insert(expr_id, new_constraint);
        Ok(())
    }
}

impl From<HashMap<ExprId, TypeConstraint>> for TypeConstraintTable {
    fn from(value: HashMap<ExprId, TypeConstraint>) -> Self {
        Self { table: value }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeConstraint {
    Independent { ptr: TypePtr },
    Dependent { ptr: TypePtr, constrained_by: ExprId },
}

impl TypeConstraint {
    pub fn get_ptr(&self) -> &TypePtr {
        match self {
            TypeConstraint::Independent { ptr } => ptr,
            TypeConstraint::Dependent { ptr, constrained_by: _ } => ptr,
        }
    }
}

pub struct TypeConstraintBuilder {
    table: TypeConstraintTable,
    vars: HashMap<VarId, ExprId>,
    logs: Vec<TypeLog>,
}

impl TypeConstraintBuilder {
    fn collect_log<T>(&mut self, result: TypeResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(log) => {
                self.logs.push(log);
                None
            },
        }
    }

    pub fn build(hir: &hir::Hir) -> (TypeConstraintTable, Vec<TypeLog>) {
        let mut builder = TypeConstraintBuilder {
            table: TypeConstraintTable::new(),
            vars: HashMap::new(),
            logs: Vec::new(),
        };
        hir.items.iter().for_each(|(_, item)| builder.build_item(item));
        (builder.table, builder.logs)
    }

    pub fn build_item(&mut self, item: &hir::Item) {
        match item {
            hir::Item::FnDecl(decl) => {
                for arg in &decl.body.args {
                    self.build_formal_arg(arg);
                }
                for expr in &decl.body.exprs {
                    self.build_expr(&decl.body, expr);
                }
            },
        }
    }

    pub fn build_formal_arg(&mut self, arg: &hir::FormalArgDef) {
        self.table.constrain_independent(arg.expr_id, (&arg.r#type).into());
    }

    pub fn build_expr(&mut self, body: &hir::Body, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => match literal {
                token::Literal::Bool { value: _ } => {
                    self.table.constrain_independent(expr.id, Type::Prim(ast::PrimType::Bool));
                },
                _ => unimplemented!(),
            },
            hir::ExprKind::VarDef(var_id) => {
                match body.vars.get(var_id.into_usize()) {
                    Some(var_def) => {
                        if let Some(r#type) = &var_def.r#type {
                            self.table.constrain_independent(expr.id, r#type.into());
                        }
                        match &var_def.init {
                            Some(init_expr) => {
                                self.build_expr(body, init_expr);
                                let result = self.table.constrain_dependent(expr.id, init_expr.id);
                                self.collect_log(result);
                            },
                            None => self.table.constrain(expr.id),
                        }
                        self.vars.insert(*var_id, expr.id);
                    },
                    None => unreachable!("unknown variable id"),
                }
            },
            hir::ExprKind::VarBind(bind) => {
                let def_expr_id = *self.vars.get(&bind.var_id).unwrap(); // fix unwrap
                self.table.constrain_independent(expr.id, Type::Void);
                self.build_expr(body, &bind.value);
                let result = self.table.constrain_dependent(def_expr_id, bind.value.id);
                self.collect_log(result);
            },
            hir::ExprKind::LocalRef(local_id) => match local_id {
                LocalId::FormalArg(arg_id) => match body.args.get(arg_id.into_usize()) {
                    Some(arg_def) => {
                        let result = self.table.constrain_dependent(expr.id, arg_def.expr_id);
                        self.collect_log(result);
                    },
                    None => panic!("unknown argument id"),
                },
                LocalId::Var(_) => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}
