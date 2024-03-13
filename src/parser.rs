pub mod ast;

use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::token::*;
use self::ast::*;

pub struct BodyScopeHierarchy {
    scopes: Vec<BodyScope>,
}

impl BodyScopeHierarchy {
    pub fn new() -> BodyScopeHierarchy {
        BodyScopeHierarchy { scopes: Vec::new() }
    }

    pub fn get_current_scope(&self) -> &BodyScope {
        self.scopes.last().expect("could not get current body scope")
    }

    pub fn get_current_scope_mut(&mut self) -> &mut BodyScope {
        self.scopes.last_mut().expect("could not get current body scope")
    }

    pub fn get_current_symbol_table(&self) -> &LocalSymbolTable {
        &self.get_current_scope().symbol_table
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(BodyScope::new());
    }

    pub fn leave_scope(&mut self) -> LocalSymbolTable {
        self.scopes.pop().expect("could not leave body scope").exit()
    }

    pub fn declare(&mut self, id: &str, entity: LocalEntity) -> LocalSymbol {
        self.get_current_scope_mut().declare(id, entity)
    }

    pub fn resolve(&self, id: &str) -> Option<LocalSymbol> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(symbol) = each_scope.resolve(id) {
                return Some(symbol);
            }
        }

        None
    }
}

pub struct BodyScope {
    symbol_table: LocalSymbolTable,
    symbol_counter: usize,
    scopes: Vec<LocalScope>,
}

impl BodyScope {
    pub fn new() -> BodyScope {
        BodyScope {
            symbol_table: LocalSymbolTable::new(),
            symbol_counter: 0,
            scopes: vec![LocalScope::new()],
        }
    }

    pub fn exit(self) -> LocalSymbolTable {
        self.symbol_table
    }

    pub fn get_current_scope_mut(&mut self) -> &mut LocalScope {
        self.scopes.last_mut().expect("could not get current local scope")
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(LocalScope::new());
    }

    pub fn leave_scope(&mut self) {
        self.scopes.pop().expect("could not leave local scope");
    }

    pub fn declare(&mut self, id: &str, entity: LocalEntity) -> LocalSymbol {
        let symbol = LocalSymbol::from(self.symbol_counter);
        self.get_current_scope_mut().declare(id, symbol.clone());
        self.symbol_table.insert(symbol.clone(), entity);
        self.symbol_counter += 1;
        symbol
    }

    pub fn resolve(&self, id: &str) -> Option<LocalSymbol> {
        for each_scope in self.scopes.iter().rev() {
            if let Some(symbol) = each_scope.resolve(id) {
                return Some(symbol);
            }
        }

        None
    }
}

pub struct LocalScope {
    id_decls: Vec<(String, LocalSymbol)>,
}

impl LocalScope {
    pub fn new() -> LocalScope {
        LocalScope {
            id_decls: Vec::new(),
        }
    }

    pub fn declare(&mut self, id: &str, symbol: LocalSymbol) {
        self.id_decls.push((id.to_string(), symbol));
    }

    pub fn resolve(&self, id: &str) -> Option<LocalSymbol> {
        for (each_id, each_symbol) in self.id_decls.iter().rev() {
            if each_id == id {
                return Some(each_symbol.clone());
            }
        }

        None
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {
    ExpectedExpr { span: Span },
    ExpectedId { span: Span },
    ExpectedItem { span: Span },
    ExpectedFormalArg { span: Span },
    ExpectedKeyword { keyword: Keyword, span: Span },
    ExpectedToken { kind: TokenKind, span: Span },
    ExpectedType { span: Span },
    UnexpectedEof { span: Span },
}

pub type ParserResult<T> = Result<T, ParserLog>;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    module_path: GlobalSymbol,
    pub(crate) global_symbol_table: GlobalSymbolTable,
    pub(crate) body_scope_hierarchy: BodyScopeHierarchy,
    last_token_span: Span,
    logs: Vec<ParserLog>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>, module_path: GlobalSymbol) -> Parser<'a> {
        Parser {
            tokens: tokens.iter().peekable(),
            module_path,
            global_symbol_table: GlobalSymbolTable::new(),
            body_scope_hierarchy: BodyScopeHierarchy::new(),
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

    pub fn next_line(&mut self) {
        let line = self.get_next_span().line;
        self.consume_until_before(|next| next.span.line > line);
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

    pub fn parse(mut self) -> (Ast, Vec<ParserLog>) {
        self.parse_items();
        let ast = Ast { global_symbol_table: Box::new(self.global_symbol_table) };
        (ast, self.logs)
    }

    pub fn parse_items(&mut self) {
        loop {
            if self.is_eof() {
                break;
            }

            let item_result = self.parse_single_item();
            if let Some((id, entity)) = self.record_result_log(item_result) {
                let symbol = self.module_path.clone().add(&id.id);
                self.global_symbol_table.insert(symbol, entity);
            }
        }
    }

    pub fn parse_single_item(&mut self) -> ParserResult<(Id, GlobalEntity)> {
        let span = self.get_next_span();

        let (id, entity) = if self.consume_keyword(Keyword::Fn).is_some() {
            let (_, id) = self.expect_id()?;
            let args = self.parse_formal_args()?;
            let ret_type = if self.is_next_eq(TokenKind::OpenCurlyBracket).is_some() {
                None
            } else {
                Some(self.parse_type()?)
            };
            let (body, symbol_table) = self.parse_body()?;
            let decl = FnDecl { id: id.clone(), args, ret_type, body, symbol_table: Box::new(symbol_table) };
            (id, GlobalEntity::FnDecl(decl))
        } else {
            self.next_line();
            return Err(ParserLog::ExpectedItem { span });
        };

        Ok((id, entity))
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
                let log = ParserLog::ExpectedExpr { span: self.get_next_span() };
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

    pub fn parse_body(&mut self) -> ParserResult<(Vec<Expr>, LocalSymbolTable)> {
        self.body_scope_hierarchy.enter_scope();
        let result = self.parse_body_();
        let symbol_table = self.body_scope_hierarchy.leave_scope();
        result.map(|body| (body, symbol_table))
    }

    fn parse_body_(&mut self) -> ParserResult<Vec<Expr>> {
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

        Ok(exprs)
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
            self.expect_any()?;
            let span = id.span.clone();
            let symbol = self.body_scope_hierarchy.resolve(&id.id);
            let expr = Expr {
                kind: Box::new(ExprKind::Id(id, symbol)),
                span,
            };
            Ok(expr)
        } else if self.is_next_keyword(Keyword::Let).is_some() {
            self.parse_var_decl_or_init()
        } else {
            Err(ParserLog::ExpectedExpr { span: self.get_next_span() })
        }
    }

    pub fn parse_var_decl_or_init(&mut self) -> ParserResult<Expr> {
        self.expect_keyword(Keyword::Let)?;
        let (_, id) = self.expect_id()?;
        let span = id.span.clone();
        let str_id = id.id.clone();

        // todo: let 式のセミコロンの扱いを検討する（暫定的にセミコロン必須で実装）
        let symbol = if self.is_next_eq(TokenKind::Semicolon).is_some() {
            // e.g.) let i;
            let decl = VarDecl { id, r#type: None };
            self.body_scope_hierarchy.declare(&str_id, LocalEntity::VarDecl(decl))
        } else if self.consume(TokenKind::Equal).is_some() {
            // e.g.) let i = 0;
            let expr = self.parse_expr()?;
            let init = VarInit { id, r#type: None, expr };
            self.body_scope_hierarchy.declare(&str_id, LocalEntity::VarInit(init))
        } else {
            let r#type = Some(self.parse_type()?);
            if self.is_next_eq(TokenKind::Semicolon).is_some() {
                // e.g.) let i usize;
                let decl = VarDecl { id, r#type };
                self.body_scope_hierarchy.declare(&str_id, LocalEntity::VarDecl(decl))
            } else if self.consume(TokenKind::Equal).is_some() {
                // e.g.) let i usize = 0;
                let expr = self.parse_expr()?;
                let init = VarInit { id, r#type, expr };
                self.body_scope_hierarchy.declare(&str_id, LocalEntity::VarInit(init))
            } else {
                return Err(ParserLog::ExpectedToken { kind: TokenKind::Semicolon, span: self.get_next_span() });
            }
        };

        let expr = Expr { kind: Box::new(ExprKind::LocalEntity(symbol)), span };
        Ok(expr)
    }
}
