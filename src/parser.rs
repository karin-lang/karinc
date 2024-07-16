pub mod ast;
pub mod log;

use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::token::*;
use crate::parser::ast::*;
use crate::parser::log::{ParserLog, ParserResult};
use crate::hir::id::*;

#[derive(Clone, Debug, PartialEq)]
pub struct ParserHakoContext {
    declared_names: Vec<String>,
    hako_id: HakoId,
    next_item_id: usize,
}

impl ParserHakoContext {
    pub fn new(hako_id: HakoId) -> ParserHakoContext {
        ParserHakoContext { declared_names: Vec::new(), hako_id, next_item_id: 0 }
    }

    pub fn declare(&mut self, id: Id) -> ParserResult<ItemId> {
        if self.declared_names.contains(&id.id) {
            let span = id.span.clone();
            Err(ParserLog::DuplicateItemName { id, span })
        } else {
            self.declared_names.push(id.id);
            let new_id = self.generate_item_id();
            Ok(new_id)
        }
    }

    fn generate_item_id(&mut self) -> ItemId {
        let next = self.next_item_id;
        self.next_item_id += 1;
        ItemId::new(self.hako_id.into_usize(), next)
    }
}

pub enum OperationExprResult {
    Expr(Expr),
    ExprWithOperators(Vec<OperationElem>, Span),
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    hako_context: &'a mut ParserHakoContext,
    last_token_span: Span,
    last_body_id: &'a mut usize,
    logs: Vec<ParserLog>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>, hako_context: &'a mut ParserHakoContext, last_body_id: &'a mut usize) -> Parser<'a> {
        Parser {
            tokens: tokens.iter().peekable(),
            hako_context,
            last_token_span: tokens.last().map(|token| token.span.clone()).unwrap_or_default(),
            last_body_id,
            logs: Vec::new(),
        }
    }

    pub fn clone_tokens_ptr(&mut self) -> Peekable<Iter<'a, Token>> {
        self.tokens.clone()
    }

    pub fn set_tokens_ptr(&mut self, ptr: Peekable<Iter<'a, Token>>) {
        self.tokens = ptr;
    }

    pub fn get_next_span(&mut self) -> Span {
        self.tokens.peek().map(|next| next.span.clone()).unwrap_or(self.last_token_span.clone())
    }

    pub fn is_eof(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    pub fn is_next(&mut self, f: impl FnOnce(&&&Token) -> bool) -> Option<&&Token> {
        let next = self.tokens.peek();
        next.filter(f)
    }

    pub fn is_next_eq(&mut self, kind: TokenKind) -> Option<&&Token> {
        self.is_next(|v| v.kind == kind)
    }

    pub fn is_next_id(&mut self) -> Option<Id> {
        let next = self.tokens.peek();

        match next {
            Some(token) => match &token.kind {
                TokenKind::Id(id) => Some(Id { id: id.clone(), span: token.span.clone() }),
                _ => None,
            },
            None => None,
        }
    }

    pub fn is_next_keyword(&mut self, keyword: Keyword) -> Option<&&Token> {
        self.is_next_eq(TokenKind::Keyword(keyword))
    }

    pub fn get_next_literal(&mut self) -> Option<&Literal> {
        match self.peek() {
            Some(next) => match &next.kind {
                TokenKind::Keyword(keyword) if *keyword == Keyword::Void => Some(&Literal::Void),
                TokenKind::Literal(literal) => Some(literal),
                _ => None,
            },
            None => None,
        }
    }

    pub fn consume(&mut self, kind: TokenKind) -> Option<&Token> {
        self
            .tokens
            .next_if(|next_token| next_token.kind == kind)
    }

    pub fn consume_id(&mut self) -> Option<(&Token, Id)> {
        let value = if let Some(token) = self.tokens.peek() {
            if let TokenKind::Id(id) = &token.kind {
                let id = Id { id: id.clone(), span: token.span.clone() };
                (*token, id)
            } else {
                return None;
            }
        } else {
            return None;
        };

        self.tokens.next();
        Some(value)
    }

    pub fn consume_keyword(&mut self, keyword: Keyword) -> Option<&Token> {
        self
            .tokens
            .next_if(|next_token| next_token.kind == TokenKind::Keyword(keyword))
    }

    pub fn consume_until(&mut self, f: impl Fn(&Token) -> bool) {
        while let Some(next) = self.tokens.next() {
            if f(next) {
                break;
            }
        }
    }

    pub fn consume_until_before(&mut self, f: impl Fn(&Token) -> bool) {
        while let Some(next) = self.tokens.peek() {
            if f(next) {
                break;
            }

            self.tokens.next();
        }
    }

    pub fn peek(&mut self) -> Option<&&Token> {
        self.tokens.peek()
    }

    pub fn expect(&mut self, kind: TokenKind) -> ParserResult<()> {
        if let Some(next) = self.tokens.peek() {
            if next.kind == kind {
                self.tokens.next();
                Ok(())
            } else {
                Err(ParserLog::ExpectedToken { kind, span: next.span.clone() })
            }
        } else {
            Err(ParserLog::ExpectedToken { kind, span: self.last_token_span.clone() })
        }
    }

    pub fn expect_any(&mut self) -> ParserResult<&Token> {
        self.tokens.next().ok_or(ParserLog::UnexpectedEof { span: self.last_token_span.clone() })
    }

    pub fn expect_id(&mut self) -> ParserResult<(&Token, Id)> {
        let value = if let Some(token) = self.tokens.peek() {
            if let TokenKind::Id(id) = &token.kind {
                let id = Id { id: id.clone(), span: token.span.clone() };
                (*token, id)
            } else {
                return Err(ParserLog::ExpectedId { span: token.span.clone() })
            }
        } else {
            return Err(ParserLog::ExpectedId { span: self.last_token_span.clone() })
        };

        self.tokens.next();
        Ok(value)
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> ParserResult<()> {
        if let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Keyword(next_keyword) if *next_keyword == keyword => {
                    self.tokens.next();
                    Ok(())
                },
                _ => Err(ParserLog::ExpectedKeyword { keyword, span: token.span.clone() }),
            }
        } else {
            Err(ParserLog::ExpectedKeyword { keyword, span: self.last_token_span.clone() })
        }
    }

    pub fn generate_body_id(&mut self) -> BodyId {
        let new_id = BodyId::new(*self.last_body_id);
        *self.last_body_id += 1;
        new_id
    }

    pub fn get_logs(&self) -> &Vec<ParserLog> {
        &self.logs
    }

    pub fn record_log(&mut self, log: ParserLog) {
        self.logs.push(log);
    }

    pub fn record_result_log<T>(&mut self, result: ParserResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(log) => {
                self.record_log(log);
                None
            }
        }
    }

    pub fn parse(mut self, mod_id: ModId, mod_path: Path) -> (Ast, Vec<ParserLog>) {
        let items = self.parse_items();
        let ast = Ast { mod_id, mod_path, items };
        (ast, self.logs)
    }

    pub fn parse_body_or_block(&mut self) -> ParserResult<Vec<Expr>> {
        self.expect(TokenKind::OpenCurlyBracket)?;
        let mut exprs = Vec::new();

        loop {
            if self.is_eof() {
                break;
            }

            if self.consume(TokenKind::ClosingCurlyBracket).is_some() {
                break;
            }

            let new_expr = match self.parse_expr() {
                Ok(v) => v,
                Err(log) => {
                    // Consumes until before expression/body end if there is a problem.
                    self.consume_until_before(|token| token.kind == TokenKind::Semicolon || token.kind == TokenKind::ClosingCurlyBracket);
                    return Err(log);
                },
            };
            let semicolon_result = self.expect(TokenKind::Semicolon);
            self.record_result_log(semicolon_result);
            exprs.push(new_expr);
        }

        Ok(exprs)
    }

    pub fn parse_items(&mut self) -> Vec<Item> {
        let mut items = Vec::new();
        loop {
            if self.is_eof() {
                break;
            }
            let item_result = self.parse_single_item();
            match self.record_result_log(item_result) {
                Some(new_item) => items.push(new_item),
                // Exit the current item if there is a problem in it.
                None => self.consume_until(|token| token.kind == TokenKind::ClosingCurlyBracket),
            }
        }
        items
    }

    pub fn parse_single_item(&mut self) -> ParserResult<Item> {
        let markers = self.parse_marker()?;
        let beginning_span = self.get_next_span();
        let accessibility = if self.consume_keyword(Keyword::Pub).is_some() {
            Accessibility::Pub
        } else {
            Accessibility::Default
        };
        let item = if self.consume_keyword(Keyword::Fn).is_some() {
            let (_, name) = self.expect_id()?;
            let id = self.hako_context.declare(name.clone())?;
            let args = self.parse_formal_args()?;
            let ret_type = if self.is_next_eq(TokenKind::OpenCurlyBracket).is_some() {
                None
            } else {
                Some(self.parse_type()?)
            };
            let body = self.parse_body(ret_type, args)?;
            let decl = FnDecl { body };
            Item { id, name, markers, accessibility, kind: ItemKind::FnDecl(decl) }
        } else {
            return Err(ParserLog::ExpectedItem { span: beginning_span });
        };
        Ok(item)
    }

    pub fn parse_marker(&mut self) -> ParserResult<Vec<Marker>> {
        let mut markers = Vec::new();
        while let Some(_) = self.consume(TokenKind::At) {
            let (name_token, name) = self.expect_id()?;
            let span = name_token.span.clone();
            let kind = match name.id.as_str() {
                "sysembed" => {
                    let (_, embed_name) = self.expect_id()?;
                    MarkerKind::SysEmbed { name: embed_name.id.clone() }
                },
                _ => return Err(ParserLog::UnknownMarkerName { span }),
            };
            let new_marker = Marker { kind, span };
            markers.push(new_marker);
        }
        Ok(markers)
    }

    pub fn consume_ref_mut(&mut self) -> RefMut {
        if self.consume_keyword(Keyword::Ref).is_some() {
            RefMut::Ref
        } else if self.consume_keyword(Keyword::Mut).is_some() {
            RefMut::Mut
        } else {
            RefMut::None
        }
    }

    pub fn parse_formal_args(&mut self) -> ParserResult<Vec<FormalArg>> {
        self.expect(TokenKind::OpenParen)?;
        let mut args = Vec::new();
        let mut allow_next_arg = true;

        loop {
            if self.is_eof() {
                break;
            }

            if self.consume(TokenKind::ClosingParen).is_some() {
                break;
            }

            if let Some(comma) = self.consume(TokenKind::Comma) {
                let log = ParserLog::ExpectedFormalArg { span: comma.span.clone() };
                self.record_log(log);
            }

            if !allow_next_arg {
                let log = ParserLog::ExpectedFormalArg { span: self.get_next_span() };
                self.record_log(log);
                self.consume_until(|next| next.kind == TokenKind::ClosingParen);
                break;
            }

            let (_, id) = self.expect_id()?;
            let ref_mut = self.consume_ref_mut();
            let r#type = self.parse_type()?;
            allow_next_arg = self.consume(TokenKind::Comma).is_some();
            let new_arg = FormalArg { id, ref_mut, r#type };
            args.push(new_arg);
        }

        Ok(args)
    }

    pub fn parse_body(&mut self, ret_type: Option<Type>, args: Vec<FormalArg>) -> ParserResult<Body> {
        let exprs = self.parse_body_or_block()?;
        let id = self.generate_body_id();
        let body = Body { id, ret_type, args, exprs };
        Ok(body)
    }

    pub fn parse_type(&mut self) -> ParserResult<Type> {
        let first_token = self.expect_any()?;

        match &first_token.kind {
            TokenKind::Id(id) => {
                let id = Id { id: id.clone(), span: first_token.span.clone() };
                let kind = TypeKind::Id(id);
                let r#type = Type { kind: Box::new(kind), span: first_token.span.clone() };
                Ok(r#type)
            },
            TokenKind::Keyword(keyword) if *keyword == Keyword::Void => {
                let kind = TypeKind::Prim(PrimType::Void);
                let r#type = Type { kind: Box::new(kind), span: first_token.span.clone() };
                Ok(r#type)
            },
            TokenKind::PrimType(prim_type) => {
                let kind = TypeKind::Prim(*prim_type);
                let r#type = Type { kind: Box::new(kind), span: first_token.span.clone() };
                Ok(r#type)
            },
            _ => Err(ParserLog::ExpectedType { span: first_token.span.clone() }),
        }
    }

    pub fn parse_expr(&mut self) -> ParserResult<Expr> {
        self._parse_operation_expr()
    }

    // 通常の式もしくは中置演算子つきの演算式をパースする
    fn _parse_operation_expr(&mut self) -> ParserResult<Expr> {
        let first_expr = self.__parse_operation_expr()?;
        let mut op_stack: Vec<BinaryOperator> = Vec::new();

        let (mut elems, span) = match first_expr {
            // 演算式でなければ通常の式として返す
            OperationExprResult::Expr(expr) => return Ok(expr),
            // 演算式であれば中置演算子のパースに進む
            OperationExprResult::ExprWithOperators(expr, span) => (expr, span),
        };

        loop {
            // 中置演算子をパース
            match self.consume_infix_operator()? {
                Some(op) => {
                    if let Some(last_op) = op_stack.last() {
                        // 新しい演算子がスタック内の最新の演算子よりも優先度が高い場合は何もしない
                        // そうでない場合はスタックに演算子をプッシュする
                        if op.get_precedence() <= last_op.get_precedence() {
                            elems.push(OperationElem::Operator(Operator::Binary(*last_op)));
                            op_stack.pop().unwrap();
                            // 先ほどと同じようにもう一度プッシュできる演算子がないか優先度を確かめる
                            if let Some(last_op) = op_stack.last() {
                                if op.get_precedence() <= last_op.get_precedence() {
                                    elems.push(OperationElem::Operator(Operator::Binary(*last_op)));
                                    op_stack.pop().unwrap();
                                }
                            }
                        }
                    }
                    op_stack.push(op);
                    match self.__parse_operation_expr()? {
                        OperationExprResult::Expr(expr) => elems.push(OperationElem::Term(expr)),
                        OperationExprResult::ExprWithOperators(mut expr, _) => elems.append(&mut expr),
                    }
                },
                _ => break, // どの演算子にもマッチしないため演算式の終端と判断して終了する
            }
        }

        while let Some(op) = op_stack.pop() {
            elems.push(OperationElem::Operator(Operator::Binary(op)));
        }

        let expr = Expr {
            kind: ExprKind::Operation(Box::new(Operation { elems })),
            span,
        };
        Ok(expr)
    }

    // 通常の式もしくは前置/後置演算子つきの演算式をパースする
    fn __parse_operation_expr(&mut self) -> ParserResult<OperationExprResult> {
        // 前置演算子をパース
        let prefix_op = self.consume_prefix_operator()?;
        // 式をパース
        let expr = self._parse_expr()?;
        let span = expr.span.clone();
        // 後置演算子をパース
        let postfix_op = self.consume_postfix_operator()?;
        // 中置演算子つきの演算式か判断する (式の後ろに中置演算子があるかを判定)
        let has_infix_operator = self.is_next_infix_operator();
        // 結果を返す
        let result = if prefix_op.is_some() || postfix_op.is_some() || has_infix_operator {
            let mut elems = vec![OperationElem::Term(expr)];
            if let Some(op) = postfix_op {
                elems.push(OperationElem::Operator(Operator::Unary(op)));
            }
            if let Some(op) = prefix_op {
                elems.push(OperationElem::Operator(Operator::Unary(op)));
            }
            // 前置/中置/後置のいずれかの演算子が見つかることで演算式と判断されれば ExprWithOperators を返す
            OperationExprResult::ExprWithOperators(elems, span)
        } else {
            // 演算式でなければ Expr を返す
            OperationExprResult::Expr(expr)
        };
        Ok(result)
    }

    fn consume_prefix_operator(&mut self) -> ParserResult<Option<UnaryOperator>> {
        if let Some(next) = self.peek() {
            match Operator::to_prefix_operator(next) {
                Some(v) => {
                    self.expect_any()?;
                    Ok(Some(v))
                },
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn is_next_infix_operator(&mut self) -> bool {
        match self.peek() {
            Some(next) => Operator::to_infix_operator(next).is_some(),
            None => false,
        }
    }

    fn consume_infix_operator(&mut self) -> ParserResult<Option<BinaryOperator>> {
        if let Some(next) = self.peek() {
            match Operator::to_infix_operator(next) {
                Some(v) => {
                    self.expect_any()?;
                    Ok(Some(v))
                },
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn consume_postfix_operator(&mut self) -> ParserResult<Option<UnaryOperator>> {
        if let Some(next) = self.peek() {
            match Operator::to_postfix_operator(next) {
                Some(v) => {
                    self.expect_any()?;
                    Ok(Some(v))
                },
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    fn _parse_expr(&mut self) -> ParserResult<Expr> {
        let beginning_span = self.get_next_span();
        // todo: match 式で token_kind を判断して条件分岐を最適化できないか検討する
        if let Some(_) = self.is_next_id() {
            if let Some((bind, span)) = self.parse_var_bind()? {
                // 束縛式 (id = value) を処理する
                let expr = Expr { kind: ExprKind::VarBind(bind), span };
                Ok(expr)
            } else {
                let segments =  self.parse_id_segments()?;
                let kind = if self.is_next_eq(TokenKind::OpenParen).is_some() {
                    // 丸括弧がパスに続く場合は関数として処理する
                    ExprKind::FnCall(
                        FnCall {
                            path: Path::from(segments),
                            args: self.parse_actual_args()?,
                        },
                    )
                } else {
                    // セグメントが1つの場合 ID として、2つ以上の場合はパスとして扱う
                    match segments.get(0) {
                        Some(id) if segments.len() == 1 => {
                            let id = Id { id: id.clone(), span: beginning_span.clone() };
                            ExprKind::Id(id)
                        },
                        _ => ExprKind::Path(Path::from(segments)),
                    }
                };
                let expr = Expr { kind, span: beginning_span };
                Ok(expr)
            }
        } else if self.is_next_keyword(Keyword::Ret).is_some() {
            let ret = self.parse_ret()?;
            let expr = Expr { kind: ExprKind::Ret(ret), span: beginning_span };
            Ok(expr)
        } else if self.is_next_keyword(Keyword::Let).is_some() {
            let (def, span) = self.parse_var_def()?;
            let expr = Expr { kind: ExprKind::VarDef(def), span };
            Ok(expr)
        } else if self.is_next_keyword(Keyword::If).is_some() {
            let r#if = self.parse_if()?;
            let expr = Expr { kind: ExprKind::If(r#if), span: beginning_span };
            Ok(expr)
        } else if self.is_next_keyword(Keyword::For).is_some() {
            let r#for = self.parse_for()?;
            let expr = Expr { kind: ExprKind::For(r#for), span: beginning_span };
            Ok(expr)
        } else if let Some(literal) = self.get_next_literal().cloned() {
            // todo: add test
            self.expect_any()?;
            let expr = Expr { kind: ExprKind::Literal(literal), span: beginning_span };
            Ok(expr)
        } else if let Some(_) = self.is_next_eq(TokenKind::OpenCurlyBracket) {
            let exprs = self.parse_body_or_block()?;
            let block = Block { exprs };
            let expr = Expr { kind: ExprKind::Block(block), span: beginning_span };
            Ok(expr)
        } else {
            Err(ParserLog::ExpectedExpr { span: beginning_span })
        }
    }

    // 1つのセグメントを持つ ID または、2つ以上のセグメントを持つパスをパースする
    pub fn parse_id_segments(&mut self) -> ParserResult<Vec<String>> {
        let (_, first_id) = self.expect_id()?;
        let mut segments = vec![first_id.id];
        while let Some(_) = self.consume(TokenKind::DoubleColon) {
            let (_, new_id) = self.expect_id()?;
            segments.push(new_id.id);
        }
        Ok(segments)
    }

    pub fn parse_block(&mut self) -> ParserResult<Block> {
        let exprs = self.parse_body_or_block()?;
        let block = Block { exprs };
        Ok(block)
    }

    pub fn parse_actual_args(&mut self) -> ParserResult<Vec<ActualArg>> {
        self.expect(TokenKind::OpenParen)?;
        let mut args = Vec::new();
        let mut allow_next_arg = true;

        loop {
            if self.is_eof() {
                break;
            }

            if self.consume(TokenKind::ClosingParen).is_some() {
                break;
            }

            if let Some(comma) = self.consume(TokenKind::Comma) {
                let log = ParserLog::ExpectedActualArg { span: comma.span.clone() };
                self.record_log(log);
            }

            if !allow_next_arg {
                let log = ParserLog::ExpectedActualArg { span: self.get_next_span() };
                self.record_log(log);
                self.consume_until(|next| next.kind == TokenKind::ClosingParen);
                break;
            }

            let ref_mut = self.consume_ref_mut();
            let expr = self.parse_expr()?;
            allow_next_arg = self.consume(TokenKind::Comma).is_some();
            let new_arg = ActualArg { ref_mut, expr };
            args.push(new_arg);
        }

        Ok(args)
    }

    pub fn parse_ret(&mut self) -> ParserResult<Ret> {
        self.expect_keyword(Keyword::Ret)?;
        let value = self.parse_expr()?;
        let ret = Ret { value: Box::new(value) };
        Ok(ret)
    }

    pub fn parse_var_def(&mut self) -> ParserResult<(VarDef, Span)> {
        self.expect_keyword(Keyword::Let)?;
        let ref_mut = self.consume_ref_mut();
        let (_, id) = self.expect_id()?;
        let span = id.span.clone();
        // todo: let 式のセミコロンの扱いを検討する（暫定的にセミコロン必須で実装）
        let def = if self.is_next_eq(TokenKind::Semicolon).is_some() {
            // e.g.) let i;
            VarDef { id, ref_mut, r#type: None, init: None }
        } else if self.consume(TokenKind::Equal).is_some() {
            // e.g.) let i = 0;
            let expr = self.parse_expr()?;
            VarDef { id, ref_mut, r#type: None, init: Some(Box::new(expr)) }
        } else {
            let r#type = Some(self.parse_type()?);
            if self.is_next_eq(TokenKind::Semicolon).is_some() {
                // e.g.) let i usize;
                VarDef { id, ref_mut, r#type, init: None }
            } else if self.consume(TokenKind::Equal).is_some() {
                // e.g.) let i usize = 0;
                let expr = self.parse_expr()?;
                VarDef { id, ref_mut, r#type, init: Some(Box::new(expr)) }
            } else {
                return Err(ParserLog::ExpectedToken { kind: TokenKind::Semicolon, span: self.get_next_span() });
            }
        };
        Ok((def, span))
    }

    pub fn parse_var_bind(&mut self) -> ParserResult<Option<(VarBind, Span)>> {
        let tokens_ptr = self.clone_tokens_ptr();
        let id = match self.is_next_id() {
            Some(id) => {
                self.expect_any()?;
                id
            },
            None => return Ok(None),
        };
        if self.consume(TokenKind::Equal).is_none() {
            self.set_tokens_ptr(tokens_ptr);
            return Ok(None);
        }
        let span = id.span.clone();
        let value = self.parse_expr()?;
        let bind = VarBind { id, value: Box::new(value) };
        Ok(Some((bind, span)))
    }

    pub fn parse_if(&mut self) -> ParserResult<If> {
        self.expect_keyword(Keyword::If)?;
        let cond = self.parse_expr()?;
        let block = self.parse_block()?;
        let mut elifs = Vec::new();
        while self.consume_keyword(Keyword::Elif).is_some() {
            let elif_cond = self.parse_expr()?;
            let elif_block = self.parse_block()?;
            let new_elif = Elif { cond: Box::new(elif_cond), block: elif_block };
            elifs.push(new_elif);
        }
        let r#else = if self.consume_keyword(Keyword::Else).is_some() {
            let else_block = self.parse_block()?;
            Some(else_block)
        } else {
            None
        };
        let r#if = If { cond: Box::new(cond), block, elifs, r#else };
        Ok(r#if)
    }

    pub fn parse_for(&mut self) -> ParserResult<For> {
        self.expect_keyword(Keyword::For)?;
        let kind = if self.is_next_eq(TokenKind::OpenCurlyBracket).is_some() {
            ForKind::Endless
        } else {
            let first_expr = self.parse_expr()?;
            if self.consume_keyword(Keyword::In).is_some() {
                let range = self.parse_expr()?;
                ForKind::Range { index: Box::new(first_expr), range: Box::new(range) }
            } else {
                ForKind::Cond { cond: Box::new(first_expr) }
            }
        };
        let block = self.parse_block()?;
        let r#for = For { kind, block };
        Ok(r#for)
    }
}
