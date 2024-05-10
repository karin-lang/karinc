pub mod lower;

use std::collections::HashMap;

use crate::parser::{ast, ast::tltype::TopLevelTypeTable};
use crate::{hir, hir::id::*};
use crate::tir::r#type::*;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeLog {
    FnCallWithInvalidArgLen { expected: usize, provided: usize },
    InconsistentConstraint,
}

pub type TypeResult<T> = Result<T, TypeLog>;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstraintTable {
    table: HashMap<TypeId, TypeConstraint>,
}

impl TypeConstraintTable {
    pub fn new() -> TypeConstraintTable {
        TypeConstraintTable { table: HashMap::new() }
    }

    pub fn to_sorted_vec(&self) -> Vec<(&TypeId, &TypeConstraint)> {
        let mut vec: Vec<(&TypeId, &TypeConstraint)> = self.table.iter().collect();
        vec.sort_by(|a, b| a.0.cmp(&b.0));
        vec
    }

    pub fn is_suitable_for_key(type_id: TypeId) -> bool {
        match type_id {
            TypeId::TopLevel(_) => false,
            TypeId::FormalArg(_) => true,
            TypeId::Var(_) => true,
            TypeId::Expr(_) => true,
        }
    }

    pub fn get(&self, type_id: &TypeId) -> Option<&TypeConstraint> {
        self.table.get(&type_id)
    }

    pub fn get_mut(&mut self, type_id: &TypeId) -> Option<&mut TypeConstraint> {
        self.table.get_mut(&type_id)
    }

    pub fn contains_type_id(&self, type_id: &TypeId) -> bool {
        self.table.contains_key(type_id)
    }

    pub fn insert(&mut self, type_id: TypeId, constraint: TypeConstraint) -> Option<TypeConstraint> {
        if !TypeConstraintTable::is_suitable_for_key(type_id) {
            panic!("cannot constraint type with top level id");
        }
        self.table.insert(type_id, constraint)
    }
}

impl From<HashMap<TypeId, TypeConstraint>> for TypeConstraintTable {
    fn from(value: HashMap<TypeId, TypeConstraint>) -> Self {
        Self { table: value }
    }
}

impl Into<HashMap<TypeId, TypeConstraint>> for TypeConstraintTable {
    fn into(self) -> HashMap<TypeId, TypeConstraint> {
        self.table
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstraintBuilder<'a> {
    top_level_type_table: &'a TopLevelTypeTable,
    table: TypeConstraintTable,
}

impl<'a> TypeConstraintBuilder<'a> {
    pub fn new(top_level_type_table: &'a TopLevelTypeTable) -> TypeConstraintBuilder<'a> {
        TypeConstraintBuilder { top_level_type_table, table: TypeConstraintTable::new() }
    }

    pub fn into_table(self) -> TypeConstraintTable {
        self.table
    }

    pub fn is_consistent(type_id: &Type, constrained_by: &Type) -> bool {
        !type_id.is_resolved() || *type_id == *constrained_by
    }

    pub fn add_constraint(&mut self, type_id: TypeId) -> TypeResult<()> {
        if !self.table.contains_type_id(&type_id) {
            self.add_independent_constraint(type_id, Type::Unresolved)?;
        }
        Ok(())
    }

    pub fn add_independent_constraint(&mut self, type_id: TypeId, r#type: Type) -> TypeResult<()> {
        if let Some(constraint) = self.table.get_mut(&type_id) {
            let ptr = constraint.get_ptr().borrow();
            if !TypeConstraintBuilder::is_consistent(&*ptr, &r#type) {
                // detects inconsistent types
                return Err(TypeLog::InconsistentConstraint);
            }
        }
        let constraint = TypeConstraint::new(TypePtr::new(r#type));
        self.table.insert(type_id, constraint);
        Ok(())
    }

    pub fn add_dependent_constraint(&mut self, type_id: TypeId, constrained_by: TypeId) -> TypeResult<()> {
        match &constrained_by {
            TypeId::TopLevel(top_level_id) => {
                match self.top_level_type_table.get(top_level_id) {
                    Some(r#type) => {
                        if let Some(constraint) = self.table.get(&type_id) {
                            let ptr = constraint.get_ptr().borrow();
                            if !TypeConstraintBuilder::is_consistent(&ptr, r#type) {
                                // detects inconsistent types
                                return Err(TypeLog::InconsistentConstraint);
                            }
                        }
                        let new_constraint = TypeConstraint::new_constrained(
                            TypePtr::new(r#type.clone()),
                            Vec::new(),
                            Some(constrained_by),
                        );
                        self.table.insert(type_id, new_constraint);
                    },
                    None => panic!("unknown top level id"),
                }
            },
            _ => {
                let new_ptr = match self.table.get_mut(&constrained_by) {
                    Some(constraint) => {
                        constraint.constrain(type_id);
                        constraint.get_ptr().clone()
                    },
                    None => panic!("unknown expression id: {:?}", constrained_by),
                };
                match self.table.get_mut(&type_id) {
                    Some(constraint) => {
                        if !TypeConstraintBuilder::is_consistent(&constraint.get_ptr().borrow(), &new_ptr.borrow()) {
                            // detects inconsistent types
                            return Err(TypeLog::InconsistentConstraint);
                        }
                        constraint.set_ptr(new_ptr);
                        constraint.set_constrained_by(constrained_by);
                    },
                    None => {
                        let new_constraint = TypeConstraint::new_constrained(
                            new_ptr,
                            Vec::new(),
                            Some(constrained_by),
                        );
                        self.table.insert(type_id, new_constraint);
                    },
                }
            },
        }
        Ok(())
    }

    pub fn copy_and_be_constrained(&mut self, type_id: TypeId, source: TypeId) -> TypeResult<()> {
        let source_ptr = match self.table.get(&source) {
            Some(constraint) => constraint.get_ptr().borrow().clone(),
            None => panic!("unknown expression id"),
        };
        self.add_independent_constraint(type_id, source_ptr)?;
        self.add_dependent_constraint(source, type_id)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeConstraint {
    ptr: TypePtr,
    constrains: Vec<TypeId>,
    constrained_by: Option<TypeId>,
}

impl TypeConstraint {
    pub fn new(ptr: TypePtr) -> TypeConstraint {
        TypeConstraint { ptr, constrains: Vec::new(), constrained_by: None }
    }

    pub fn new_constrained(ptr: TypePtr, constrains: Vec<TypeId>, constrained_by: Option<TypeId>) -> TypeConstraint {
        TypeConstraint { ptr, constrains, constrained_by }
    }

    pub fn get_ptr(&self) -> &TypePtr {
        &self.ptr
    }

    pub fn set_ptr(&mut self, ptr: TypePtr) {
        self.ptr = ptr;
    }

    pub fn constrain(&mut self, constrains: TypeId) {
        self.constrains.push(constrains);
    }

    pub fn set_constrained_by(&mut self, constrained_by: TypeId) {
        self.constrained_by = Some(constrained_by);
    }
}
