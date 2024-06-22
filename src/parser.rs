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
    hako_id: HakoId,
    next_item_id: usize,
}

impl ParserHakoContext {
    pub fn new(hako_id: HakoId) -> ParserHakoContext {
        ParserHakoContext { hako_id, next_item_id: 0 }
    }

    pub fn generate_item_id(&mut self) -> ItemId {
        let next = self.next_item_id;
        self.next_item_id += 1;
        ItemId::new(self.hako_id.into_usize(), next)
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    hako_context: &'a mut ParserHakoContext,
    last_token_span: Span,
    logs: Vec<ParserLog>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>, hako_context: &'a mut ParserHakoContext) -> Parser<'a> {
        Parser {
            tokens: tokens.iter().peekable(),
            hako_context,
            last_token_span: tokens.last().map(|token| token.span.clone()).unwrap_or_default(),
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
                _ => None
            },
            None => None
        }
    }

    pub fn is_next_keyword(&mut self, keyword: Keyword) -> Option<&&Token> {
        self.is_next_eq(TokenKind::Keyword(keyword))
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

    pub fn parse_items(&mut self) -> Vec<Item> {
        let mut items = Vec::new();
        loop {
            if self.is_eof() {
                break;
            }
            let item_result = self.parse_single_item();
            if let Some(new_item) = self.record_result_log(item_result) {
                items.push(new_item);
            }
        }
        items
    }

    pub fn parse_single_item(&mut self) -> ParserResult<Item> {
        let span = self.get_next_span();
        let item = if self.consume_keyword(Keyword::Fn).is_some() {
            let id = self.hako_context.generate_item_id();
            let (_, name) = self.expect_id()?;
            let args = self.parse_formal_args()?;
            let ret_type = if self.is_next_eq(TokenKind::OpenCurlyBracket).is_some() {
                None
            } else {
                Some(self.parse_type()?)
            };
            let body = self.parse_body(ret_type, args)?;
            let decl = FnDecl { body };
            Item { id, name, kind: ItemKind::FnDecl(decl) }
        } else {
            self.consume_until(|token| token.kind == TokenKind::ClosingCurlyBracket);
            return Err(ParserLog::ExpectedItem { span });
        };
        Ok(item)
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
            let mutable = self.consume_keyword(Keyword::Mut).is_some();
            let r#type = self.parse_type()?;
            allow_next_arg = self.consume(TokenKind::Comma).is_some();
            let new_arg = FormalArg { id, r#type, mutable };
            args.push(new_arg);
        }

        Ok(args)
    }

    pub fn parse_body(&mut self, ret_type: Option<Type>, args: Vec<FormalArg>) -> ParserResult<Body> {
        self.expect(TokenKind::OpenCurlyBracket)?;
        let mut exprs = Vec::new();

        loop {
            if self.is_eof() {
                break;
            }

            if self.consume(TokenKind::ClosingCurlyBracket).is_some() {
                break;
            }

            let new_expr = self.parse_expr()?;
            let semicolon_result = self.expect(TokenKind::Semicolon);
            self.record_result_log(semicolon_result);
            exprs.push(new_expr);
        }

        let body = Body { ret_type, args, exprs };
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
            TokenKind::PrimType(prim_type) => {
                let kind = TypeKind::Prim(*prim_type);
                let r#type = Type { kind: Box::new(kind), span: first_token.span.clone() };
                Ok(r#type)
            },
            _ => Err(ParserLog::ExpectedType { span: first_token.span.clone() }),
        }
    }

    pub fn parse_expr(&mut self) -> ParserResult<Expr> {
        // todo: match 式で token_kind を判断して条件分岐を最適化できないか検討する
        if let Some(id) = self.is_next_id() {
            if let Some((bind, span)) = self.parse_var_bind()? {
                let expr = Expr { kind: ExprKind::VarBind(bind), span };
                Ok(expr)
            } else {
                self.expect_any()?;
                let span = id.span.clone();
                let kind = if self.is_next_eq(TokenKind::OpenParen).is_some() {
                    ExprKind::FnCall(
                        FnCall {
                            path: Path::from(vec![id.id.to_string()]),
                            args: self.parse_actual_args()?,
                        },
                    )
                } else {
                    ExprKind::Id(id)
                };
                let expr = Expr { kind, span };
                Ok(expr)
            }
        } else if self.is_next_keyword(Keyword::Let).is_some() {
            let (def, span) = self.parse_var_def()?;
            let expr = Expr { kind: ExprKind::VarDef(def), span };
            Ok(expr)
        } else {
            Err(ParserLog::ExpectedExpr { span: self.get_next_span() })
        }
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

            let expr = self.parse_expr()?;
            allow_next_arg = self.consume(TokenKind::Comma).is_some();
            let new_arg = ActualArg { expr };
            args.push(new_arg);
        }

        Ok(args)
    }

    pub fn parse_var_def(&mut self) -> ParserResult<(VarDef, Span)> {
        self.expect_keyword(Keyword::Let)?;
        let (_, id) = self.expect_id()?;
        let span = id.span.clone();
        // todo: let 式のセミコロンの扱いを検討する（暫定的にセミコロン必須で実装）
        let def = if self.is_next_eq(TokenKind::Semicolon).is_some() {
            // e.g.) let i;
            VarDef { id, r#type: None, init: None }
        } else if self.consume(TokenKind::Equal).is_some() {
            // e.g.) let i = 0;
            let expr = self.parse_expr()?;
            VarDef { id, r#type: None, init: Some(Box::new(expr)) }
        } else {
            let r#type = Some(self.parse_type()?);
            if self.is_next_eq(TokenKind::Semicolon).is_some() {
                // e.g.) let i usize;
                VarDef { id, r#type, init: None }
            } else if self.consume(TokenKind::Equal).is_some() {
                // e.g.) let i usize = 0;
                let expr = self.parse_expr()?;
                VarDef { id, r#type, init: Some(Box::new(expr)) }
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
}
