use std::collections::HashMap;
use std::cell::{Ref, RefCell};
use std::rc::Rc;

use crate::lexer::token;
use crate::parser::{ast, ast::tltype::TopLevelTypeTable};
use crate::{hir, hir::id::*};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeLog {
    FnCallWithInvalidArgLen { expected: usize, provided: usize },
    InconsistentConstraint,
}

pub type TypeResult<T> = Result<T, TypeLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Num,
    Int,
    Float,
    Item(ast::Path),
    Prim(ast::PrimType),
    Fn(FnType),
    Undefined,
    Unresolved,
}

impl Type {
    pub fn is_resolved(&self) -> bool {
        *self != Type::Unresolved
    }
}

impl From<&ast::Type> for Type {
    fn from(value: &ast::Type) -> Self {
        match &*value.kind {
            ast::TypeKind::Id(_) => unimplemented!(),
            ast::TypeKind::Prim(prim_type) => Type::Prim(prim_type.clone()),
        }
    }
}

impl From<&hir::Type> for Type {
    fn from(value: &hir::Type) -> Self {
        match &*value.kind {
            hir::TypeKind::Item(_) => unimplemented!(),
            hir::TypeKind::Prim(prim_type) => Type::Prim(prim_type.clone()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnType {
    pub ret_type: Box<Type>,
    pub arg_types: Vec<Type>,
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

pub struct TypeConstraintLowering<'a> {
    hir: &'a hir::Hir,
    builder: TypeConstraintBuilder<'a>,
    logs: Vec<TypeLog>,
}

impl<'a> TypeConstraintLowering<'a> {
    fn collect_log<T>(&mut self, result: TypeResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(log) => {
                self.logs.push(log);
                None
            },
        }
    }

    pub fn lower(hir: &hir::Hir, top_level_type_table: &'a TopLevelTypeTable) -> (TypeConstraintTable, Vec<TypeLog>) {
        let mut lowering = TypeConstraintLowering {
            hir,
            builder: TypeConstraintBuilder::new(top_level_type_table),
            logs: Vec::new(),
        };
        lowering.hir.items.iter().for_each(|(_, item)| lowering.lower_item(item));
        (lowering.builder.into_table(), lowering.logs)
    }

    pub fn lower_item(&mut self, item: &hir::Item) {
        match &item.kind {
            hir::ItemKind::FnDecl(decl) => {
                for (arg_id, arg) in decl.body.args.iter().enumerate() {
                    self.lower_formal_arg(FormalArgId::new(arg_id), arg);
                }
                for expr in &decl.body.exprs {
                    self.lower_expr(&decl.body, expr);
                }
            },
        }
    }

    pub fn lower_formal_arg(&mut self, arg_id: FormalArgId, arg: &hir::FormalArgDef) {
        let result = self.builder.add_independent_constraint(TypeId::FormalArg(arg_id), (&arg.r#type).into());
        self.collect_log(result);
    }

    pub fn lower_expr(&mut self, body: &hir::Body, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::Literal(literal) => {
                let r#type = match literal {
                    token::Literal::Bool { value: _ } => Type::Prim(ast::PrimType::Bool),
                    // todo: 桁などの型検査を実施する & テスト追加
                    token::Literal::Int { base: _, int_digits: _, r#type } => match r#type {
                        Some(r#type) => Type::Prim(*r#type),
                        None => Type::Int,
                    },
                    token::Literal::Float { base: _, int_digits: _, fraction_digits: _, r#type } => match r#type {
                        Some(r#type) => Type::Prim(*r#type),
                        None => Type::Float,
                    },
                    token::Literal::Char { value: _ } => Type::Prim(ast::PrimType::Char),
                    token::Literal::Str { value: _ } => Type::Prim(ast::PrimType::Str),
                    token::Literal::ByteChar { value: _ } => Type::Prim(ast::PrimType::U32),
                    // todo: 配列型を実装する & テスト追加
                    // token::Literal::ByteStr { value: _ } => Type::Prim(ast::PrimType::Arr),
                    _ => unimplemented!(),
                };
                let result = self.builder.add_independent_constraint(TypeId::Expr(expr.id), r#type);
                self.collect_log(result);
            },
            hir::ExprKind::LocalRef(local_id) => {
                let type_id = match local_id {
                    LocalId::FormalArg(id) => TypeId::FormalArg(*id),
                    LocalId::Var(id) => TypeId::Var(*id),
                };
                let result = self.builder.add_dependent_constraint(TypeId::Expr(expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::FnCall(call) => {
                let type_id = TypeId::TopLevel(TopLevelId::FnRet(call.r#fn));
                let result = self.builder.add_dependent_constraint(TypeId::Expr(expr.id), type_id);
                self.collect_log(result);

                let fn_type = match self.builder.top_level_type_table.get_fn(&call.r#fn) {
                    Some(v) => v,
                    None => unreachable!("called unknown function"),
                };
                let arg_len_match = fn_type.arg_types.len() == call.args.len();
                if !arg_len_match {
                    self.collect_log::<()>(Err(TypeLog::FnCallWithInvalidArgLen { expected: fn_type.arg_types.len(), provided: call.args.len() }));
                }
                for (i, each_arg) in call.args.iter().enumerate() {
                    self.lower_expr(body, &each_arg.expr);
                    if arg_len_match {
                        let type_id = TypeId::TopLevel(TopLevelId::FnArg(call.r#fn, FormalArgId::new(i)));
                        let result = self.builder.add_dependent_constraint(TypeId::Expr(each_arg.expr.id), type_id);
                        self.collect_log(result);
                    }
                }
            },
            hir::ExprKind::TopLevelRef(top_level_id) => {
                let type_id = TypeId::TopLevel(*top_level_id);
                let result = self.builder.add_dependent_constraint(TypeId::Expr(expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::VarDef(var_id) => {
                let var_def = match body.vars.get(var_id.into_usize()) {
                    Some(v) => v,
                    None => unreachable!("unknown variable id"),
                };
                let result = self.builder.add_independent_constraint(TypeId::Expr(expr.id), Type::Void);
                self.collect_log(result);
                if let Some(r#type) = &var_def.r#type {
                    let result = self.builder.add_independent_constraint(TypeId::Var(*var_id), r#type.into());
                    self.collect_log(result);
                }
                if let Some(init_expr) = &var_def.init {
                    self.lower_expr(body, init_expr);
                    let result = self.builder.add_dependent_constraint(TypeId::Var(*var_id), TypeId::Expr(init_expr.id));
                    self.collect_log(result);
                }
            },
            hir::ExprKind::VarBind(bind) => {
                let result = self.builder.add_independent_constraint(TypeId::Expr(expr.id), Type::Void);
                self.collect_log(result);
                self.lower_expr(body, &bind.value);
                let result = self.builder.copy_and_be_constrained(TypeId::Var(bind.var_id), TypeId::Expr(bind.value.id));
                self.collect_log(result);
            },
        }
    }
}
