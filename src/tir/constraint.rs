use core::panic;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::ast;
use crate::{hir, hir::id::*};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Item(ast::Path),
    Prim(ast::PrimType),
    Undefined,
}

impl From<&hir::Type> for Type {
    fn from(value: &hir::Type) -> Self {
        match &*value.kind {
            hir::TypeKind::Path(_) => unimplemented!(),
            hir::TypeKind::Prim(prim_type) => Type::Prim(prim_type.clone()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstraintTable {
    table: HashMap<ExprId, TypeConstraintPtr>,
}

impl TypeConstraintTable {
    pub fn new() -> TypeConstraintTable {
        TypeConstraintTable { table: HashMap::new() }
    }

    pub fn constrain_with_type(&mut self, expr_id: ExprId, r#type: Type) {
        let constraint = TypeConstraintPtr::new(TypeConstraint::Type(r#type));
        self.table.insert(expr_id, constraint);
    }

    pub fn constrain_with_expr(&mut self, expr_id: ExprId, constrained_by: ExprId) {
        let constraint = match self.table.get(&constrained_by) {
            Some(constraint) => {
                TypeConstraintPtr::new(TypeConstraint::Expr { constrained_by, ptr: constraint.clone() })
            },
            None => panic!("unknown expression id"),
        };
        self.table.insert(expr_id, constraint);
    }

    pub fn constrain_with_not_derived(&mut self, expr_id: ExprId) {
        let constraint = TypeConstraintPtr::new(TypeConstraint::NotDerived);
        self.table.insert(expr_id, constraint);
    }
}

impl From<HashMap<ExprId, TypeConstraintPtr>> for TypeConstraintTable {
    fn from(value: HashMap<ExprId, TypeConstraintPtr>) -> Self {
        Self { table: value }
    }
}

#[derive(PartialEq)]
pub struct TypeConstraintPtr(Rc<RefCell<TypeConstraint>>);

impl TypeConstraintPtr {
    pub fn new(constraint: TypeConstraint) -> TypeConstraintPtr {
        TypeConstraintPtr(Rc::new(RefCell::new(constraint)))
    }
}

impl Clone for TypeConstraintPtr {
    fn clone(&self) -> Self {
        TypeConstraintPtr(Rc::clone(&self.0))
    }
}

impl std::fmt::Debug for TypeConstraintPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", *self.0.borrow())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeConstraint {
    Type(Type),
    Expr { constrained_by: ExprId, ptr: TypeConstraintPtr },
    NotDerived,
}

pub struct TypeConstraintBuilder {
    table: TypeConstraintTable,
    vars: HashMap<VarId, ExprId>,
}

impl TypeConstraintBuilder {
    pub fn build(hir: &hir::Hir) -> TypeConstraintTable {
        let mut builder = TypeConstraintBuilder {
            table: TypeConstraintTable::new(),
            vars: HashMap::new(),
        };
        hir.items.iter().for_each(|(_, item)| builder.build_item(item));
        builder.table
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
        self.table.constrain_with_type(arg.expr_id, (&arg.r#type).into());
    }

    pub fn build_expr(&mut self, body: &hir::Body, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::VarDef(var_id) => {
                match body.vars.get(var_id.into_usize()) {
                    Some(var_def) => {
                        match &var_def.init {
                            Some(init_expr) => {
                                self.build_expr(body, init_expr);
                                self.table.constrain_with_expr(expr.id, init_expr.id);
                            },
                            None => self.table.constrain_with_not_derived(expr.id),
                        }
                        self.vars.insert(*var_id, expr.id);
                    },
                    None => unreachable!("unknown variable id"),
                }
            },
            hir::ExprKind::LocalRef(local_id) => match local_id {
                LocalId::FormalArg(arg_id) => match body.args.get(arg_id.into_usize()) {
                    Some(arg_def) => {
                        self.table.constrain_with_expr(expr.id, arg_def.expr_id);
                    },
                    None => panic!("unknown argument id"),
                },
                LocalId::Var(_) => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}
