pub mod ast;

use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::token::*;
use self::ast::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {
    ExpectedExpr { span: Span },
    ExpectedId { span: Span },
    ExpectedItem { span: Span },
    ExpectedKeyword { span: Span, keyword: Keyword },
    ExpectedToken { span: Span, kind: TokenKind },
    ExpectedType { span: Span },
    UnexpectedEof { span: Span },
}

pub type ParserResult<T> = Result<T, ParserLog>;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    last_token_span: Span,
    logs: Vec<ParserLog>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            tokens: tokens.iter().peekable(),
            last_token_span: tokens.last().map(|token| token.span.clone()).unwrap_or_default(),
            logs: Vec::new(),
        }
    }

    pub fn get_next_span(&mut self) -> Span {
        self.tokens.peek().map(|next| next.span.clone()).unwrap_or(self.last_token_span.clone())
    }

    pub fn is_eof(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    pub fn is_next(&mut self, kind: TokenKind) -> bool {
        self.tokens.peek().is_some_and(|v| v.kind == kind)
    }

    pub fn is_next_keyword(&mut self, keyword: Keyword) -> bool {
        self.is_next(TokenKind::Keyword(keyword))
    }

    pub fn next_line(&mut self) {
        // fix: 次のトークンを次の改行に修正する（トークン位置の情報を基準にする）
        self.tokens.next();
    }

    pub fn consume(&mut self, kind: TokenKind) -> bool {
        self
            .tokens
            .next_if(|next_token| next_token.kind == kind)
            .is_some()
    }

    pub fn consume_id(&mut self) -> Option<Id> {
        if let Some(token) = self.tokens.peek() {
            if let TokenKind::Id(id) = &token.kind {
                self.tokens.next();
                return Some(id.into());
            }
        }

        None
    }

    pub fn consume_keyword(&mut self, keyword: Keyword) -> bool {
        self
            .tokens
            .next_if(|next_token| next_token.kind == TokenKind::Keyword(keyword))
            .is_some()
    }

    pub fn consume_until(&mut self, f: impl Fn(&Token) -> bool) {
        while let Some(next) = self.tokens.next() {
            if f(next) {
                break;
            }
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
                Err(ParserLog::ExpectedToken { span: next.span.clone(), kind })
            }
        } else {
            Err(ParserLog::ExpectedToken { span: self.last_token_span.clone(), kind })
        }
    }

    pub fn expect_any(&mut self) -> ParserResult<&Token> {
        self.tokens.next().ok_or(ParserLog::UnexpectedEof { span: self.last_token_span.clone() })
    }

    pub fn expect_id(&mut self) -> ParserResult<Id> {
        if let Some(token) = self.tokens.peek() {
            if let TokenKind::Id(id) = &token.kind {
                self.tokens.next();
                Ok(id.into())
            } else {
                Err(ParserLog::ExpectedId { span: token.span.clone() })
            }
        } else {
            Err(ParserLog::ExpectedId { span: self.last_token_span.clone() })
        }
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> ParserResult<()> {
        if let Some(token) = self.tokens.peek() {
            match &token.kind {
                TokenKind::Keyword(next_keyword) if *next_keyword == keyword => {
                    self.tokens.next();
                    Ok(())
                },
                _ => Err(ParserLog::ExpectedKeyword { span: token.span.clone(), keyword }),
            }
        } else {
            Err(ParserLog::ExpectedKeyword { span: self.last_token_span.clone(), keyword })
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

    pub fn parse(mut self) -> (Ast, Vec<ParserLog>) {
        let items = self.parse_items();
        let ast = Ast { items };
        (ast, self.logs)
    }

    pub fn parse_items(&mut self) -> Vec<Item> {
        let mut items = Vec::new();

        loop {
            if self.is_eof() {
                break;
            }

            let item_result = self.parse_single_item();

            if let Some(item) = self.record_result_log(item_result) {
                items.push(item);
            }
        }

        items
    }

    pub fn parse_single_item(&mut self) -> ParserResult<Item> {
        let begin_span = self.get_next_span();

        let kind = if self.consume_keyword(Keyword::Fn) {
            let id = self.expect_id()?;
            let args = self.parse_formal_args()?;
            let ret_type = if self.is_next(TokenKind::OpenCurlyBracket) {
                None
            } else {
                Some(self.parse_type()?)
            };
            let body = self.parse_body()?;
            let decl = FnDecl { id, args, ret_type, body };
            ItemKind::FnDecl(decl)
        } else {
            // todo: もっといいトークンの進め先を考える
            self.next_line();
            return Err(ParserLog::ExpectedItem { span: begin_span });
        };

        let item = Item { kind: Box::new(kind) };
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

            if self.consume(TokenKind::ClosingParen) {
                break;
            }

            if !allow_next_arg {
                let log = ParserLog::ExpectedExpr { span: self.get_next_span() };
                self.record_log(log);
                self.consume_until(|next| next.kind == TokenKind::ClosingParen || next.kind == TokenKind::OpenCurlyBracket);
                break;
            }

            let id = self.expect_id()?;
            let r#type = self.parse_type()?;
            allow_next_arg = self.consume(TokenKind::Comma);
            let new_arg = FormalArg { id, r#type };
            args.push(new_arg);
        }

        Ok(args)
    }

    pub fn parse_body(&mut self) -> ParserResult<Vec<Expr>> {
        self.expect(TokenKind::OpenCurlyBracket)?;
        let mut exprs = Vec::new();

        loop {
            if self.is_eof() {
                break;
            }

            if self.consume(TokenKind::ClosingCurlyBracket) {
                break;
            }

            let new_expr = self.parse_expr()?;
            self.expect(TokenKind::Semicolon)?;
            exprs.push(new_expr);
        }

        Ok(exprs)
    }

    pub fn parse_type(&mut self) -> ParserResult<Type> {
        let first_token = self.expect_any()?;

        if let TokenKind::Keyword(keyword) = &first_token.kind {
            let prim_type = match keyword {
                Keyword::Usize => PrimType::Usize,
                _ => return Err(ParserLog::ExpectedType { span: first_token.span.clone() }),
            };

            let kind = TypeKind::Prim(prim_type);
            let r#type = Type { kind: Box::new(kind) };
            Ok(r#type)
        } else {
            Err(ParserLog::ExpectedType { span: first_token.span.clone() })
        }
    }

    pub fn parse_expr(&mut self) -> ParserResult<Expr> {
        if self.consume_keyword(Keyword::Let) {
            self.parse_var_decl_or_init_expr()
        } else {
            Err(ParserLog::ExpectedExpr { span: self.get_next_span() })
        }
    }

    pub fn parse_var_decl_or_init_expr(&mut self) -> ParserResult<Expr> {
        self.expect_keyword(Keyword::Let)?;
        let id = self.expect_id()?;

        // todo: let 式のセミコロンの扱いを検討する（暫定的にセミコロン必須で実装）
        let expr = if self.is_next(TokenKind::Semicolon) {
            // e.g.) let i;
            let decl = VarDecl { id, r#type: None };
            let kind = ExprKind::VarDecl(decl);
            Expr { kind: Box::new(kind) }
        } else if self.consume(TokenKind::Equal) {
            // e.g.) let i = 0;
            let expr = self.parse_expr()?;
            let init = VarInit { id, r#type: None, expr };
            let kind = ExprKind::VarInit(init);
            Expr { kind: Box::new(kind) }
        } else {
            let r#type = Some(self.parse_type()?);
            if self.is_next(TokenKind::Semicolon) {
                // e.g.) let i usize;
                let decl = VarDecl { id, r#type };
                let kind = ExprKind::VarDecl(decl);
                Expr { kind: Box::new(kind) }
            } else if self.consume(TokenKind::Equal) {
                // e.g.) let i usize = 0;
                let expr = self.parse_expr()?;
                let init = VarInit { id, r#type, expr };
                let kind = ExprKind::VarInit(init);
                Expr { kind: Box::new(kind) }
            } else {
                return Err(ParserLog::ExpectedToken { span: self.get_next_span(), kind: TokenKind::Semicolon });
            }
        };

        Ok(expr)
    }
}
