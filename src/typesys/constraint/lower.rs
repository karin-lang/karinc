use super::*;
use crate::lexer::token;
use crate::hir::id::*;

pub struct TypeConstraintLowering<'a> {
    hir: &'a hir::Hir,
    builder: TypeConstraintBuilder<'a>,
    current_mod_id: Option<ModId>,
    current_item_id: Option<ItemId>,
}

impl<'a> TypeConstraintLowering<'a> {
    fn collect_log<T>(&mut self, result: TypeResult<T>) -> Option<T> {
        let mod_id = self.current_mod_id.expect("current module id is not set.");
        self.builder.collect_log(mod_id, result)
    }

    pub fn lower(hir: &hir::Hir, top_level_type_table: &'a TopLevelTypeTable) -> (TypeConstraintTable, HashMap<ModId, Vec<TypeLog>>) {
        let mut lowering = TypeConstraintLowering {
            hir,
            builder: TypeConstraintBuilder::new(top_level_type_table),
            current_mod_id: None,
            current_item_id: None,
        };
        lowering.hir.items.iter().for_each(|(_, item)| lowering.lower_item(item));
        lowering.builder.finalize()
    }

    pub fn lower_item(&mut self, item: &hir::Item) {
        self.current_mod_id = Some(item.mod_id);
        self.current_item_id = Some(item.id);
        match &item.kind {
            hir::ItemKind::FnDecl(decl) => {
                for (arg_id, arg) in decl.body.args.iter().enumerate() {
                    self.lower_formal_arg(decl.body.id, FormalArgId::new(arg_id), arg);
                }
                for expr in &decl.body.exprs {
                    self.lower_expr(&decl.body, expr);
                }
            },
        }
    }

    pub fn lower_formal_arg(&mut self, body_id: BodyId, arg_id: FormalArgId, arg: &hir::FormalArgDef) {
        println!("{:?}", TypeId::FormalArg(body_id, arg_id));
        let result = self.builder.constrain_by_type(TypeId::FormalArg(body_id, arg_id), (&arg.r#type).into());
        self.collect_log(result);
    }

    pub fn lower_expr(&mut self, body: &hir::Body, expr: &hir::Expr) {
        match &expr.kind {
            // ブロックの返り値の型はブロック単位でなく式単位で関連付ける
            // 例）if-else 式の場合: if ブロック内の最後の式と else ブロック内の最後の式が関連付けられて型を検査する
            hir::ExprKind::Block(block) => self.lower_constrained_block(body, &expr.id, block),
            hir::ExprKind::Literal(literal) => {
                let r#type = match literal {
                    token::Literal::Void => Type::Prim(ast::PrimType::Void),
                    token::Literal::Bool { value: _ } => Type::Prim(ast::PrimType::Bool),
                    // todo: 桁などの型検査を実施する & テスト追加
                    token::Literal::Int { base: _, int_digits: _, r#type } => match r#type {
                        Some(r#type) => Type::Prim(*r#type),
                        None => Type::Infer(InferType::Int), // todo: uint を追加
                    },
                    token::Literal::Float { digits: _, r#type } => match r#type {
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
                let result = self.builder.constrain_by_type(TypeId::Expr(body.id, expr.id), r#type);
                self.collect_log(result);
            },
            hir::ExprKind::LocalRef(local_id) => {
                let type_id = match local_id {
                    LocalId::FormalArg(id) => TypeId::FormalArg(body.id, *id),
                    LocalId::Var(id) => TypeId::Var(body.id, *id),
                };
                let result = self.builder.constrain_by_other(TypeId::Expr(body.id, expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::Ret(ret) => {
                let result = self.builder.constrain_by_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::Void));
                self.collect_log(result);
                self.lower_expr(body, &ret.value);
                let result = self.builder.constrain_by_other(
                    TypeId::Expr(body.id, ret.value.id),
                    TypeId::TopLevel(TopLevelId::FnRet(self.current_item_id.expect("current item id is not set"))),
                );
                self.collect_log(result);
            }
            hir::ExprKind::FnCall(call) => {
                let type_id = TypeId::TopLevel(TopLevelId::FnRet(call.r#fn.unwrap())); // fix: unwrap()
                let result = self.builder.constrain_by_other(TypeId::Expr(body.id, expr.id), type_id);
                self.collect_log(result);

                let fn_type = match self.builder.top_level_type_table.get_fn(&call.r#fn.unwrap()) { // fix: unwrap()
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
                        let type_id = TypeId::TopLevel(TopLevelId::FnArg(call.r#fn.unwrap() /* fix: unwrap */, FormalArgId::new(i)));
                        let result = self.builder.constrain_by_other(TypeId::Expr(body.id, each_arg.expr.id), type_id);
                        self.collect_log(result);
                    }
                }
            },
            hir::ExprKind::TopLevelRef(top_level_id, _) => {
                let type_id = TypeId::TopLevel(*top_level_id);
                let result = self.builder.constrain_by_other(TypeId::Expr(body.id, expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::VarDef(var_id) => {
                let var_def = match body.vars.get(var_id.into_usize()) {
                    Some(v) => v,
                    None => unreachable!("unknown variable id"),
                };
                let result = self.builder.constrain_by_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::Void));
                self.collect_log(result);
                if let Some(r#type) = &var_def.r#type {
                    let result = self.builder.constrain_by_type(TypeId::Var(body.id, *var_id), r#type.into());
                    self.collect_log(result);
                }
                if let Some(init_expr) = &var_def.init {
                    self.lower_expr(body, init_expr);
                    let result = self.builder.constrain_by_other(TypeId::Var(body.id, *var_id), TypeId::Expr(body.id, init_expr.id));
                    self.collect_log(result);
                }
            },
            hir::ExprKind::VarBind(bind) => {
                let result = self.builder.constrain_by_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::Void));
                self.collect_log(result);
                self.lower_expr(body, &bind.value);
                let result = self.builder.copy_constraint(TypeId::Var(body.id, bind.var_id), TypeId::Expr(body.id, bind.value.id));
                self.collect_log(result);
            },
            hir::ExprKind::If(r#if) => {
                // todo: 条件網羅のため、body の型が void でないかつ else 節がついていない場合はエラーを吐く（理想は CFG 解析をして条件網羅を判断する）
                let result = self.builder.constrain(TypeId::Expr(body.id, expr.id));
                self.collect_log(result);
                self.lower_expr(body, &r#if.cond);
                self.lower_constraining_block(body, &expr.id, &r#if.block);
                r#if.elifs.iter().for_each(|elif| {
                    self.lower_expr(body, &elif.cond);
                    self.lower_constraining_block(body, &expr.id, &elif.block);
                });
                if let Some(block) = &r#if.r#else {
                    self.lower_constraining_block(body, &expr.id, block);
                }
            },
            hir::ExprKind::For(r#for) => {
                // todo: cond, index, range に型制約をつけてテストする (例) cond を bool 型に制約する
                match &r#for.kind {
                    hir::ForKind::Endless => (),
                    hir::ForKind::Cond { cond } => self.lower_expr(body, cond),
                    hir::ForKind::Range { index, range } => {
                        self.lower_expr(body, index);
                        self.lower_expr(body, range);
                    },
                }
                let result = self.builder.constrain(TypeId::Expr(body.id, expr.id));
                self.collect_log(result);
                self.lower_constraining_block(body, &expr.id, &r#for.block);
            },
            hir::ExprKind::Unknown => (),
        }
    }

    // 他の要素の型に制約を与えるブロック（ブロック式に用いる）
    pub fn lower_constrained_block(&mut self, body: &hir::Body, constrained_expr_id: &ExprId, block: &hir::Block) {
        block.exprs.iter().for_each(|block_expr| self.lower_expr(body, block_expr));
        match block.exprs.last() {
            Some(last_expr) => {
                let result = self.builder.constrain_by_other(TypeId::Expr(body.id, *constrained_expr_id), TypeId::Expr(body.id, last_expr.id));
                self.collect_log(result);
            },
            None => {
                let result = self.builder.constrain_by_type(TypeId::Expr(body.id, *constrained_expr_id), Type::Prim(ast::PrimType::Void));
                self.collect_log(result);
            },
        }
    }

    // ブロック内要素の型に制約を受けるブロック
    pub fn lower_constraining_block(&mut self, body: &hir::Body, constraining_expr_id: &ExprId, block: &hir::Block) {
        block.exprs.iter().for_each(|block_expr| self.lower_expr(body, block_expr));
        match block.exprs.last() {
            Some(last_expr) => {
                let result = self.builder.constrain_by_other(TypeId::Expr(body.id, last_expr.id), TypeId::Expr(body.id, *constraining_expr_id));
                self.collect_log(result);
            },
            None => {
                let result = self.builder.constrain_by_type(TypeId::Expr(body.id, *constraining_expr_id), Type::Prim(ast::PrimType::Void));
                self.collect_log(result);
            },
        }
    }
}
