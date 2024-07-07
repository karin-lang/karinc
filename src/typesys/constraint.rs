pub mod lower;

use std::collections::HashMap;

use crate::parser::{ast, ast::tltype::TopLevelTypeTable};
use crate::{hir, hir::id::*};
use crate::typesys::*;
use crate::typesys::log::{TypeLog, TypeResult};

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
            TypeId::FormalArg(_, _) => true,
            TypeId::Var(_, _) => true,
            TypeId::Expr(_, _) => true,
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
    logs: HashMap<ModId, Vec<TypeLog>>,
}

impl<'a> TypeConstraintBuilder<'a> {
    pub fn new(top_level_type_table: &'a TopLevelTypeTable) -> TypeConstraintBuilder<'a> {
        TypeConstraintBuilder {
            top_level_type_table,
            table: TypeConstraintTable::new(),
            logs: HashMap::new(),
        }
    }

    pub fn from_table(
        top_level_type_table: &'a TopLevelTypeTable,
        type_constraint_table: TypeConstraintTable,
    ) -> TypeConstraintBuilder<'a> {
        TypeConstraintBuilder {
            top_level_type_table,
            table: type_constraint_table,
            logs: HashMap::new(),
        }
    }

    pub fn into_table(self) -> TypeConstraintTable {
        self.table
    }

    fn collect_log<T>(&mut self, mod_id: ModId, result: TypeResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(new_log) => {
                match self.logs.get_mut(&mod_id) {
                    Some(v) => v.push(new_log),
                    None => {
                        let _ = self.logs.insert(mod_id, vec![new_log]);
                    },
                }
                None
            },
        }
    }

    // todo: 部分型に対応
    pub fn is_consistent(type_id: &Type, constrained_by: &Type) -> bool {
        !type_id.is_resolved() || !constrained_by.is_resolved() || *type_id == *constrained_by
    }

    pub fn constrain(&mut self, type_id: TypeId) -> TypeResult<()> {
        if !self.table.contains_type_id(&type_id) {
            self.constrain_by_type(type_id, Type::Unresolved)?;
        }
        Ok(())
    }

    pub fn constrain_by_type(&mut self, type_id: TypeId, r#type: Type) -> TypeResult<()> {
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

    pub fn constrain_by_other(&mut self, type_id: TypeId, constrained_by: TypeId) -> TypeResult<()> {
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
                    None => panic!("unknown top level id"), // todo: fix panic?
                }
            },
            _ => {
                let new_ptr = match self.table.get_mut(&constrained_by) {
                    Some(constraint) => {
                        constraint.constrain(type_id);
                        if constraint.get_ptr().borrow().is_resolved() {
                            // 制約元の型が解決済みであれば制約元の型を制約先に複製する
                            constraint.get_ptr().clone()
                        } else {
                            // 制約元の型が未解決であれば制約先の型を制約元に複製する
                            match self.table.get_mut(&type_id) {
                                Some(constraint) => constraint.get_ptr().clone(),
                                None => TypePtr::new(Type::Unknown),
                            }
                        }
                    },
                    None => TypePtr::new(Type::Unknown),
                };
                // 制約先の型を新しいポインタに設定する
                match self.table.get_mut(&type_id) {
                    Some(constraint) => {
                        if !TypeConstraintBuilder::is_consistent(&constraint.get_ptr().borrow(), &new_ptr.borrow()) {
                            // detects inconsistent types
                            return Err(TypeLog::InconsistentConstraint);
                        }
                        constraint.set_ptr(new_ptr.clone());
                        constraint.set_constrained_by(constrained_by);
                    },
                    None => {
                        let new_constraint = TypeConstraint::new_constrained(
                            new_ptr.clone(),
                            Vec::new(),
                            Some(constrained_by),
                        );
                        self.table.insert(type_id, new_constraint);
                    },
                }
                // 制約元の型を制約先と一致させる
                match self.table.get_mut(&constrained_by) {
                    Some(constraint) => constraint.set_ptr(new_ptr),
                    None => (),
                }
            },
        }
        Ok(())
    }

    // Make target dependent on source: copy source type to target and constrains source by target.
    pub fn copy_constraint(&mut self, to: TypeId, from: TypeId) -> TypeResult<()> {
        let source_ptr = match self.table.get(&from) {
            Some(constraint) => constraint.get_ptr().borrow().clone(),
            None => panic!("unknown expression id"),
        };
        self.constrain_by_type(to, source_ptr)?;
        self.constrain_by_other(from, to)?;
        Ok(())
    }

    pub fn finalize(mut self) -> (TypeConstraintTable, HashMap<ModId, Vec<TypeLog>>) {
        let mut finalization_logs = Vec::new();
        for (type_id, constraint) in self.table.to_sorted_vec() {
            let new_log = match &*constraint.get_ptr().borrow() {
                Type::Unknown => Some(TypeLog::UnknownType { type_id: *type_id }),
                Type::Unresolved => Some(TypeLog::UnresolvedType { type_id: *type_id }),
                _ => None,
            };
            if let Some(new_log) = new_log {
                finalization_logs.push(new_log);
            }
        }
        for new_log in finalization_logs {
            // fix: mod_id
            self.collect_log::<()>(ModId::new(0, 0), Err(new_log));
        }
        (self.table, self.logs)
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
