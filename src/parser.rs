pub mod ast;

use std::{collections::HashMap, iter::Peekable};
use std::slice::Iter;

use crate::lexer::{token::*, tokenize::Lexer, ModSource, SourceTree};
use self::ast::*;

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

pub struct ParsingBroker {
    items: Vec<Item>,
    node_id_gen: NodeIdGen,
}

// SourceTreeをAST用のツリーに再構築してモジュール内のアイテムのノードIDを計算する？
// モジュール、アイテムをノードIDでなく文字列で管理する？
// 文字列管理でASTを生成したあとにアイテムのIDを数値に変換する？→不可
impl ParsingBroker {
    pub fn new() -> ParsingBroker {
        ParsingBroker {
            items: Vec::new(),
            node_id_gen: NodeIdGen::new(),
        }
    }

    pub fn parse(mut self, src_tree: &SourceTree) -> Ast {
        let mut hako_mods = Vec::new();
        for each_hako in &src_tree.hakos {
            for (each_mod_id, each_mod) in &each_hako.mods {
                hako_mods.push(NodeId::from(*each_mod_id));
                self.parse_mod(each_mod);
            }
        }
        Ast { items: self.items, hako_mods }
    }

    pub fn parse_mod(&mut self, mod_src: &ModSource) {
        let mod_item = Item { node_id: mod_src, id: (), kind: () };
        self.items.push(mod_item);
        for (_, each_submod) in &mod_src.submods {
            self.parse_mod(each_submod);
        }
    }

    pub fn parse_mod_items(&mut self, mod_src: &ModSource) {
        self.items.insert();
        self.parse_mod_src(mod_src);
        for (_, each_submod) in &mod_src.submods {
            self.parse_mod(each_submod);
        }
    }

    pub fn parse_mod_src(&mut self, mod_src: &ModSource) {
        let lexer = Lexer::new();
        let (tokens, _lexer_logs) = lexer.tokenize(mod_src.src);

        let parser = Parser::new(&tokens, &mut self.node_id_gen);
        let (mut items, _parser_logs) = parser.parse();

        let submod_node_ids = mod_src.submods.iter().map(|(each_submod_id, _)| NodeId::from(*each_submod_id)).collect();
        let item_node_ids = items.iter().map(|each_item| (each_item.id.id.clone(), each_item.node_id.clone())).collect();
        let mod_item = Item {
            node_id: self.node_id_gen.generate(),
            id: Id { id: mod_src.id.clone(), span: Span::default() },
            kind: ItemKind::Mod(
                Mod { submods: submod_node_ids, items: item_node_ids },
            ),
        };

        self.non_mod_items.push(mod_item);
        self.non_mod_items.append(&mut items);
    }
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    last_token_span: Span,
    node_id_gen: &'a mut NodeIdGen,
    logs: Vec<ParserLog>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>, node_id_gen: &'a mut NodeIdGen) -> Parser<'a> {
        Parser {
            tokens: tokens.iter().peekable(),
            last_token_span: tokens.last().map(|token| token.span.clone()).unwrap_or_default(),
            node_id_gen,
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

    pub fn parse(mut self) -> (HashMap<NodeId, Item>, Vec<ParserLog>) {
        let items = self.parse_items();
        (items, self.logs)
    }

    pub fn parse_items(&mut self) -> HashMap<NodeId, Item> {
        let mut items = HashMap::new();

        loop {
            if self.is_eof() {
                break;
            }

            let item_result = self.parse_single_item();
            if let Some(new_item) = self.record_result_log(item_result) {
                items.insert(new_item.node_id.clone(), new_item);
            }
        }

        items
    }

    pub fn parse_single_item(&mut self) -> ParserResult<Item> {
        let span = self.get_next_span();

        let item = if self.consume_keyword(Keyword::Fn).is_some() {
            let node_id = self.node_id_gen.generate();
            let (_, id) = self.expect_id()?;
            let args = self.parse_formal_args()?;
            let ret_type = if self.is_next_eq(TokenKind::OpenCurlyBracket).is_some() {
                None
            } else {
                Some(self.parse_type()?)
            };
            let body = self.parse_body()?;
            let decl = FnDecl { args, ret_type, body };
            Item { node_id, id, kind: ItemKind::FnDecl(decl) }
        } else {
            self.next_line();
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

    pub fn parse_body(&mut self) -> ParserResult<Body> {
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

        let body = Body { exprs };
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
            self.expect_any()?;
            let span = id.span.clone();
            let expr = Expr { kind: ExprKind::Id(id), span };
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

        // todo: let 式のセミコロンの扱いを検討する（暫定的にセミコロン必須で実装）
        let kind = if self.is_next_eq(TokenKind::Semicolon).is_some() {
            // e.g.) let i;
            let decl = VarDecl { id, r#type: None };
            ExprKind::VarDecl(decl)
        } else if self.consume(TokenKind::Equal).is_some() {
            // e.g.) let i = 0;
            let expr = self.parse_expr()?;
            let init = VarInit { id, r#type: None, expr: Box::new(expr) };
            ExprKind::VarInit(init)
        } else {
            let r#type = Some(self.parse_type()?);
            if self.is_next_eq(TokenKind::Semicolon).is_some() {
                // e.g.) let i usize;
                let decl = VarDecl { id, r#type };
                ExprKind::VarDecl(decl)
            } else if self.consume(TokenKind::Equal).is_some() {
                // e.g.) let i usize = 0;
                let expr = self.parse_expr()?;
                let init = VarInit { id, r#type, expr: Box::new(expr) };
                ExprKind::VarInit(init)
            } else {
                return Err(ParserLog::ExpectedToken { kind: TokenKind::Semicolon, span: self.get_next_span() });
            }
        };

        let expr = Expr { kind, span };
        Ok(expr)
    }
}
