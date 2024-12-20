use std::collections::HashMap;

use crate::lexer::token::{self, Span};
use crate::hir::{self, id::*};
use crate::parser::ast::{self, tltype::TopLevelTypeTable};
use crate::typesys::{*, log::*, table::*};

pub struct TypeResolver<'a> {
    hir: &'a hir::Hir,
    builder: TypeTableBuilder<'a>,
    current_mod_id: Option<ModId>,
    current_item_id: Option<ItemId>,
}

impl<'a> TypeResolver<'a> {
    fn collect_log<T>(&mut self, result: TypeResult<T>) -> Option<T> {
        let mod_id = self.current_mod_id.expect("current module id is not set.");
        self.builder.collect_log(mod_id, result)
    }

    // arg: main_fn_path: Some の場合はメイン関数のパスを探してシグネチャ検査を実施する
    pub fn resolve(hir: &hir::Hir, top_level_type_table: &'a TopLevelTypeTable, main_fn_path: Option<&ast::Path>) -> (TypeTable, HashMap<ModId, Vec<TypeLog>>) {
        let mut lowering = TypeResolver {
            hir,
            builder: TypeTableBuilder::new(top_level_type_table),
            current_mod_id: None,
            current_item_id: None,
        };
        lowering.hir.items.iter().for_each(|(_, item)| lowering.resolve_item(item));
        lowering.validate_main_fn_signature(hir, main_fn_path);
        lowering.builder.finalize()
    }

    pub fn validate_main_fn_signature(&mut self, hir: &hir::Hir, main_fn_path: Option<&ast::Path>) {
        if let Some(main_fn_path) = main_fn_path {
            for (path, item) in &hir.items {
                if *path == *main_fn_path {
                    match &item.kind {
                        hir::ItemKind::FnDecl(decl) => {
                            if decl.body.args.len() != 0 {
                                self.builder.collect_log::<()>(item.mod_id, Err(TypeLog::ExpectedMainFnArgsToBeZeroLen { span: Span::new(0, 0) /* fix: span */ }));
                            }
                            if let Some(r#type) = &decl.body.ret_type {
                                if *r#type.kind != hir::TypeKind::Prim(ast::PrimType::None) {
                                    self.builder.collect_log::<()>(item.mod_id, Err(TypeLog::ExpectedMainFnRetTypeToBeNone { span: Span::new(0, 0) /* fix: span */ }));
                                }
                            }
                        },
                    }
                    return;
                }
            }
            self.builder.collect_log::<()>(ModId::new(0, 0), Err(TypeLog::MainFnIsNotFound));
        }
    }

    pub fn resolve_item(&mut self, item: &hir::Item) {
        self.current_mod_id = Some(item.mod_id);
        self.current_item_id = Some(item.id);
        match &item.kind {
            hir::ItemKind::FnDecl(decl) => {
                for (arg_id, arg) in decl.body.args.iter().enumerate() {
                    self.resolve_formal_arg(decl.body.id, FormalArgId::new(arg_id), arg);
                }
                for expr in &decl.body.exprs {
                    self.resolve_expr(&decl.body, expr);
                }
            },
        }
    }

    pub fn resolve_formal_arg(&mut self, body_id: BodyId, arg_id: FormalArgId, arg: &hir::FormalArgDef) {
        let result = self.builder.determine_type(TypeId::FormalArg(body_id, arg_id), (&arg.r#type).into());
        self.collect_log(result);
    }

    pub fn resolve_expr(&mut self, body: &hir::Body, expr: &hir::Expr) {
        match &expr.kind {
            // todo: 型制約を改善してテストを追加する
            hir::ExprKind::Operation(operation) => match &**operation {
                hir::Operation::Unary { operator, term } => match operator {
                    ast::UnaryOperator::Not => {
                        let result = self.builder.determine_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::Bool));
                        self.collect_log(result);
                        let result = self.builder.determine_type(TypeId::Expr(body.id, term.id), Type::Prim(ast::PrimType::Bool));
                        self.collect_log(result);
                    },
                    ast::UnaryOperator::None => {
                        let result = self.builder.determine_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::None));
                        self.collect_log(result);
                        let result = self.builder.assume_unknown(TypeId::Expr(body.id, term.id));
                        self.collect_log(result);
                    },
                },
                hir::Operation::Binary { operator, left_term, right_term } => match operator {
                    ast::BinaryOperator::Add |
                    ast::BinaryOperator::Sub |
                    ast::BinaryOperator::Mul |
                    ast::BinaryOperator::Div => {
                        let result = self.builder.determine_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::Usize));
                        self.collect_log(result);
                        let result = self.builder.constrain_with(TypeId::Expr(body.id, left_term.id), TypeId::Expr(body.id, expr.id));
                        self.collect_log(result);
                        let result = self.builder.constrain_with(TypeId::Expr(body.id, right_term.id), TypeId::Expr(body.id, expr.id));
                        self.collect_log(result);
                    },
                },
            },
            // ブロックの返り値の型はブロック単位でなく式単位で関連付ける
            // 例）if-else 式の場合: if ブロック内の最後の式と else ブロック内の最後の式が関連付けられて型を検査する
            hir::ExprKind::Block(block) => self.resolve_constrained_block(body, &expr.id, block),
            hir::ExprKind::Literal(literal) => {
                let r#type = match literal {
                    token::Literal::None => Type::Prim(ast::PrimType::None),
                    token::Literal::Bool { value: _ } => Type::Prim(ast::PrimType::Bool),
                    // todo: 桁などの型検査を実施する & テスト追加
                    token::Literal::Int { base: _, int_digits: _, r#type } => match r#type {
                        Some(r#type) => Type::Prim(*r#type),
                        None => Type::Ambiguous(AmbiguousType::Int), // todo: AmbiguousType::UnsignedInt を作成
                    },
                    token::Literal::Float { digits: _, r#type } => match r#type {
                        Some(r#type) => Type::Prim(*r#type),
                        None => Type::Ambiguous(AmbiguousType::Float),
                    },
                    token::Literal::Char { value: _ } => Type::Prim(ast::PrimType::Char),
                    token::Literal::Str { value: _ } => Type::Prim(ast::PrimType::Str),
                    token::Literal::ByteChar { value: _ } => Type::Prim(ast::PrimType::U32),
                    // todo: 配列型を実装する & テスト追加
                    // token::Literal::ByteStr { value: _ } => Type::Prim(ast::PrimType::Arr),
                    _ => unimplemented!(),
                };
                let result = self.builder.determine_type(TypeId::Expr(body.id, expr.id), r#type);
                self.collect_log(result);
            },
            hir::ExprKind::LocalRef(local_id) => {
                let type_id = match local_id {
                    LocalId::FormalArg(id) => TypeId::FormalArg(body.id, *id),
                    LocalId::Var(id) => TypeId::Var(body.id, *id),
                };
                let result = self.builder.constrain_with(TypeId::Expr(body.id, expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::Ret(ret) => {
                let result = self.builder.determine_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::None));
                self.collect_log(result);
                self.resolve_expr(body, &ret.value);
                let result = self.builder.constrain_with(
                    TypeId::Expr(body.id, ret.value.id),
                    TypeId::TopLevel(TopLevelId::FnRet(self.current_item_id.expect("current item id is not set"))),
                );
                self.collect_log(result);
            }
            hir::ExprKind::FnCall(call) => match &call.r#fn {
                Some((item_id, _)) => {
                    let type_id = TypeId::TopLevel(TopLevelId::FnRet(*item_id));
                    let result = self.builder.constrain_with(TypeId::Expr(body.id, expr.id), type_id);
                    self.collect_log(result);

                    let fn_type = match self.builder.top_level_type_table.get_fn(item_id) {
                        Some(v) => v,
                        None => unreachable!("called unknown function"),
                    };
                    let arg_len_match = fn_type.arg_types.len() == call.args.len();
                    if !arg_len_match {
                        self.collect_log::<()>(Err(TypeLog::FnCallWithInvalidArgLen { expected: fn_type.arg_types.len(), provided: call.args.len() }));
                    }
                    for (i, each_arg) in call.args.iter().enumerate() {
                        self.resolve_expr(body, &each_arg.expr);
                        if arg_len_match {
                            let type_id = TypeId::TopLevel(TopLevelId::FnArg(*item_id, FormalArgId::new(i)));
                            let result = self.builder.constrain_with(TypeId::Expr(body.id, each_arg.expr.id), type_id);
                            self.collect_log(result);
                        }
                    }
                },
                None => {
                    let result = self.builder.assume_unknown(TypeId::Expr(body.id, expr.id));
                    self.collect_log(result);
                },
            },
            hir::ExprKind::TopLevelRef(top_level_id, _) => {
                let type_id = TypeId::TopLevel(*top_level_id);
                let result = self.builder.constrain_with(TypeId::Expr(body.id, expr.id), type_id);
                self.collect_log(result);
            },
            hir::ExprKind::VarDef(var_id) => {
                let var_def = match body.vars.get(var_id.into_usize()) {
                    Some(v) => v,
                    None => unreachable!("unknown variable id"),
                };
                let result = self.builder.determine_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::None));
                self.collect_log(result);
                if let Some(r#type) = &var_def.r#type {
                    let result = self.builder.determine_type(TypeId::Var(body.id, *var_id), r#type.into());
                    self.collect_log(result);
                }
                if let Some(init_expr) = &var_def.init {
                    self.resolve_expr(body, init_expr);
                    let result = self.builder.constrain_with(TypeId::Var(body.id, *var_id), TypeId::Expr(body.id, init_expr.id));
                    self.collect_log(result);
                }
            },
            hir::ExprKind::VarBind(bind) => {
                let result = self.builder.determine_type(TypeId::Expr(body.id, expr.id), Type::Prim(ast::PrimType::None));
                self.collect_log(result);
                self.resolve_expr(body, &bind.value);
                // 制約方向としては通常「バインド値が変数の型を制約する」ことになるが「変数がバインド値の型を制約する」ようにする
                //   ※同一変数に複数回バインドされた場合でも、変数の型の被制約要素 constrained_with を単一に維持するために制約方向を逆向きにする
                let result = self.builder.copy_type_and_constrain_with(TypeId::Expr(body.id, bind.value.id), TypeId::Var(body.id, bind.var_id));
                self.collect_log(result);
            },
            hir::ExprKind::If(r#if) => {
                // todo: 条件網羅のため、body の型が none でないかつ else 節がついていない場合はエラーを吐く（理想は CFG 解析をして条件網羅を判断する）
                let result = self.builder.assume_unknown(TypeId::Expr(body.id, expr.id));
                self.collect_log(result);
                self.resolve_expr(body, &r#if.cond);
                self.resolve_constraining_block(body, &expr.id, &r#if.block);
                r#if.elifs.iter().for_each(|elif| {
                    self.resolve_expr(body, &elif.cond);
                    self.resolve_constraining_block(body, &expr.id, &elif.block);
                });
                if let Some(block) = &r#if.r#else {
                    self.resolve_constraining_block(body, &expr.id, block);
                }
            },
            hir::ExprKind::For(r#for) => {
                // todo: cond, index, range に型制約をつけてテストする (例) cond を bool 型に制約する
                match &r#for.kind {
                    hir::ForKind::Endless => (),
                    hir::ForKind::Cond { cond } => self.resolve_expr(body, cond),
                    hir::ForKind::Range { index, range } => {
                        self.resolve_expr(body, index);
                        self.resolve_expr(body, range);
                    },
                }
                let result = self.builder.assume_unknown(TypeId::Expr(body.id, expr.id));
                self.collect_log(result);
                self.resolve_constraining_block(body, &expr.id, &r#for.block);
            },
            // todo: 型制約を追加する (never型?)
            hir::ExprKind::Marker(_) => (),
            // todo: 型制約を追加する
            hir::ExprKind::Unknown => (),
        }
    }

    // 他の要素の型に制約を与えるブロック（ブロック式に用いる）
    pub fn resolve_constrained_block(&mut self, body: &hir::Body, constrained_expr_id: &ExprId, block: &hir::Block) {
        block.exprs.iter().for_each(|block_expr| self.resolve_expr(body, block_expr));
        match block.exprs.last() {
            Some(last_expr) => {
                let result = self.builder.constrain_with(TypeId::Expr(body.id, *constrained_expr_id), TypeId::Expr(body.id, last_expr.id));
                self.collect_log(result);
            },
            None => {
                let result = self.builder.determine_type(TypeId::Expr(body.id, *constrained_expr_id), Type::Prim(ast::PrimType::None));
                self.collect_log(result);
            },
        }
    }

    // ブロック内要素の型に制約を受けるブロック
    pub fn resolve_constraining_block(&mut self, body: &hir::Body, constraining_expr_id: &ExprId, block: &hir::Block) {
        block.exprs.iter().for_each(|block_expr| self.resolve_expr(body, block_expr));
        match block.exprs.last() {
            Some(last_expr) => {
                let result = self.builder.constrain_with(TypeId::Expr(body.id, last_expr.id), TypeId::Expr(body.id, *constraining_expr_id));
                self.collect_log(result);
            },
            None => {
                let result = self.builder.determine_type(TypeId::Expr(body.id, *constraining_expr_id), Type::Prim(ast::PrimType::None));
                self.collect_log(result);
            },
        }
    }
}
