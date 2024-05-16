use super::*;
use crate::lexer::token;
use crate::hir::id::*;

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
        let result = self.builder.constrain_by_type(TypeId::FormalArg(arg_id), (&arg.r#type).into());
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
                        None => Type::Infer(InferType::Int), // todo: uint を追加
                    },
                    token::Literal::Float { base: _, int_digits: _, fraction_digits: _, r#type } => match r#type {
                        Some(r#type) => Type::Prim(*r#type),
                        None => Type::Infer(InferType::Float),
                    },
                    token::Literal::Char { value: _ } => Type::Prim(ast::PrimType::Char),
                    token::Literal::Str { value: _ } => Type::Prim(ast::PrimType::Str),
                    token::Literal::ByteChar { value: _ } => Type::Prim(ast::PrimType::U32),
                    // todo: 配列型を実装する & テスト追加
                    // token::Literal::ByteStr { value: _ } => Type::Prim(ast::PrimType::Arr),
                    _ => unimplemented!(),
                };
                let result = self.builder.constrain_by_type(TypeId::Expr(expr.id), r#type);
                self.collect_log(result);
            },
            hir::ExprKind::LocalRef(local_id) => {
                let type_id = match local_id {
                    LocalId::FormalArg(id) => TypeId::FormalArg(*id),
                    LocalId::Var(id) => TypeId::Var(*id),
                };
                let result = self.builder.constrain_by_other(TypeId::Expr(expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::FnCall(call) => {
                let type_id = TypeId::TopLevel(TopLevelId::FnRet(call.r#fn));
                let result = self.builder.constrain_by_other(TypeId::Expr(expr.id), type_id);
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
                        let result = self.builder.constrain_by_other(TypeId::Expr(each_arg.expr.id), type_id);
                        self.collect_log(result);
                    }
                }
            },
            hir::ExprKind::TopLevelRef(top_level_id) => {
                let type_id = TypeId::TopLevel(*top_level_id);
                let result = self.builder.constrain_by_other(TypeId::Expr(expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::VarDef(var_id) => {
                let var_def = match body.vars.get(var_id.into_usize()) {
                    Some(v) => v,
                    None => unreachable!("unknown variable id"),
                };
                let result = self.builder.constrain_by_type(TypeId::Expr(expr.id), Type::Void);
                self.collect_log(result);
                if let Some(r#type) = &var_def.r#type {
                    let result = self.builder.constrain_by_type(TypeId::Var(*var_id), r#type.into());
                    self.collect_log(result);
                }
                if let Some(init_expr) = &var_def.init {
                    self.lower_expr(body, init_expr);
                    let result = self.builder.constrain_by_other(TypeId::Var(*var_id), TypeId::Expr(init_expr.id));
                    self.collect_log(result);
                }
            },
            hir::ExprKind::VarBind(bind) => {
                let result = self.builder.constrain_by_type(TypeId::Expr(expr.id), Type::Void);
                self.collect_log(result);
                self.lower_expr(body, &bind.value);
                let result = self.builder.copy_constraint(TypeId::Var(bind.var_id), TypeId::Expr(bind.value.id));
                self.collect_log(result);
            },
        }
    }
}
