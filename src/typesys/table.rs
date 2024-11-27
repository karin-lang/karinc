use std::collections::HashMap;

use crate::parser::ast::tltype::TopLevelTypeTable;
use crate::hir::id::*;
use crate::typesys::*;
use crate::typesys::log::{TypeLog, TypeResult};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeTable {
    table: HashMap<TypeId, TypeConstraint>,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        TypeTable { table: HashMap::new() }
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
        if !TypeTable::is_suitable_for_key(type_id) {
            panic!("cannot constraint type with top level id");
        }
        self.table.insert(type_id, constraint)
    }
}

impl From<HashMap<TypeId, TypeConstraint>> for TypeTable {
    fn from(value: HashMap<TypeId, TypeConstraint>) -> Self {
        Self { table: value }
    }
}

impl Into<HashMap<TypeId, TypeConstraint>> for TypeTable {
    fn into(self) -> HashMap<TypeId, TypeConstraint> {
        self.table
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeTableBuilder<'a> {
    pub top_level_type_table: &'a TopLevelTypeTable,
    table: TypeTable,
    logs: HashMap<ModId, Vec<TypeLog>>,
}

impl<'a> TypeTableBuilder<'a> {
    pub fn new(top_level_type_table: &'a TopLevelTypeTable) -> TypeTableBuilder<'a> {
        TypeTableBuilder {
            top_level_type_table,
            table: TypeTable::new(),
            logs: HashMap::new(),
        }
    }

    pub fn from_table(
        top_level_type_table: &'a TopLevelTypeTable,
        type_table: TypeTable,
    ) -> TypeTableBuilder<'a> {
        TypeTableBuilder {
            top_level_type_table,
            table: type_table,
            logs: HashMap::new(),
        }
    }

    pub fn into_table(self) -> TypeTable {
        self.table
    }

    pub fn collect_log<T>(&mut self, mod_id: ModId, result: TypeResult<T>) -> Option<T> {
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

    // target を一時的に不明型で決定する (型解決終了時までに解決されなかった場合は解決不可能な型としてエラーが発生する)
    pub fn assume_unknown(&mut self, target: TypeId) -> TypeResult<()> {
        if !self.table.contains_type_id(&target) {
            self.determine_type(target, Type::Unresolved)?;
        }
        Ok(())
    }

    // target の型を決定する
    pub fn determine_type(&mut self, target: TypeId, r#type: Type) -> TypeResult<()> {
        if let Some(constraint) = self.table.get_mut(&target) {
            let ptr = constraint.get_ptr().borrow();
            if !ptr.is_consistent(&r#type) {
                // detects inconsistent types
                return Err(TypeLog::InconsistentConstraint);
            }
        }
        let constraint = TypeConstraint::new(TypePtr::new(r#type));
        self.table.insert(target, constraint);
        Ok(())
    }

    // target の型を他要素の型で制約する
    pub fn constrain_with(&mut self, target: TypeId, constrain_with: TypeId) -> TypeResult<()> {
        if let TypeId::TopLevel(top_level_id) = &constrain_with {
            match self.top_level_type_table.get(top_level_id) {
                Some(type_constrain_with) => {
                    if let Some(target_constraint) = self.table.get(&target) {
                        let target_ptr = target_constraint.get_ptr().borrow();
                        if !target_ptr.is_consistent(type_constrain_with) {
                            // detects inconsistent types
                            return Err(TypeLog::InconsistentConstraint);
                        }
                    }
                    let new_constraint = TypeConstraint::new_constrained(
                        TypePtr::new(type_constrain_with.clone()),
                        Vec::new(),
                        Some(constrain_with),
                    );
                    self.table.insert(target, new_constraint);
                },
                None => panic!("unknown top level id"), // todo: fix panic?
            }
        } else {
            let new_ptr = match self.table.get_mut(&constrain_with) {
                Some(constraint) => {
                    constraint.constrain(target);
                    if constraint.get_ptr().borrow().is_resolved() {
                        // 制約元の型が解決済みであれば制約元の型を制約先に複製する
                        constraint.get_ptr().clone()
                    } else {
                        // 制約元の型が未解決であれば制約先の型を制約元に複製する
                        match self.table.get_mut(&target) {
                            Some(constraint) => constraint.get_ptr().clone(),
                            None => TypePtr::new(Type::Unknown),
                        }
                    }
                },
                None => TypePtr::new(Type::Unknown),
            };
            // 制約先の型を新しいポインタに設定する
            match self.table.get_mut(&target) {
                Some(constraint) => {
                    if !constraint.get_ptr().borrow().is_consistent(&new_ptr.borrow()) {
                        // detects inconsistent types
                        return Err(TypeLog::InconsistentConstraint);
                    }
                    constraint.set_ptr(new_ptr.clone());
                    constraint.constrain_with(constrain_with);
                },
                None => {
                    let new_constraint = TypeConstraint::new_constrained(
                        new_ptr.clone(),
                        Vec::new(),
                        Some(constrain_with),
                    );
                    self.table.insert(target, new_constraint);
                },
            }
            // 制約元の型を制約先と一致させる
            match self.table.get_mut(&constrain_with) {
                Some(constraint) => constraint.set_ptr(new_ptr),
                None => (),
            }
        }
        Ok(())
    }

    // target と constrain_with の型を同一化した上で制約を付与する
    // 本来とは逆方向の制約を付与する際、制約要素の型を決定した上で制約を付与するために使用する
    // 前提: target の型が決定済みであること
    pub fn copy_type_and_constrain_with(&mut self, target: TypeId, constrain_with: TypeId) -> TypeResult<()> {
        // 型のコピーにより双方の型を同一化する
        let target_ptr = self.clone_type_ptr(target);
        self.determine_type(constrain_with, target_ptr)?;
        // target を constrain_with によって制約する
        self.constrain_with(target, constrain_with)?;
        Ok(())
    }

    pub fn clone_type_ptr(&mut self, target: TypeId) -> Type {
        match self.table.get(&target) {
            Some(constraint) => constraint.get_ptr().borrow().clone(),
            None => panic!("unknown expression id"),
        }
    }

    pub fn finalize(mut self) -> (TypeTable, HashMap<ModId, Vec<TypeLog>>) {
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
    constrained_with: Option<TypeId>,
}

impl TypeConstraint {
    pub fn new(ptr: TypePtr) -> TypeConstraint {
        TypeConstraint { ptr, constrains: Vec::new(), constrained_with: None }
    }

    pub fn new_constrained(ptr: TypePtr, constrains: Vec<TypeId>, constrained_with: Option<TypeId>) -> TypeConstraint {
        TypeConstraint { ptr, constrains, constrained_with }
    }

    pub fn get_ptr(&self) -> &TypePtr {
        &self.ptr
    }

    pub fn set_ptr(&mut self, ptr: TypePtr) {
        self.ptr = ptr;
    }

    pub fn constrain(&mut self, target: TypeId) {
        self.constrains.push(target);
    }

    pub fn constrain_with(&mut self, constrain_with: TypeId) {
        self.constrained_with = Some(constrain_with);
    }
}
