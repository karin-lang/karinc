pub mod ast;

use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::token::*;
use self::ast::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {
    ExpectedToken,
    ExpectedItem,
    ExpectedExpr,
    ExpectedType,
    UnexpectedEof,
}

pub type ParserResult<T> = Result<T, ParserLog>;

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
    logs: Vec<ParserLog>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Parser<'a> {
        Parser {
            tokens: tokens.iter().peekable(),
            logs: Vec::new(),
        }
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

    pub fn peek(&mut self) -> Option<&&Token> {
        self.tokens.peek()
    }

    pub fn expect(&mut self, kind: TokenKind) -> ParserResult<()> {
        if self.consume(kind) {
            Ok(())
        } else {
            Err(ParserLog::ExpectedToken)
        }
    }

    pub fn expect_any(&mut self) -> ParserResult<&Token> {
        self.tokens.next().ok_or(ParserLog::UnexpectedEof)
    }

    pub fn expect_id(&mut self) -> ParserResult<Id> {
        self.consume_id().ok_or(ParserLog::ExpectedToken)
    }

    pub fn expect_keyword(&mut self, keyword: Keyword) -> ParserResult<()> {
        if self.consume_keyword(keyword) {
            Ok(())
        } else {
            Err(ParserLog::ExpectedToken)
        }
    }

    pub fn get_logs(&self) -> &Vec<ParserLog> {
        &self.logs
    }

    pub fn record_log<T>(&mut self, result: ParserResult<T>) -> Option<T> {
        match result {
            Ok(v) => Some(v),
            Err(log) => {
                self.logs.push(log);
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

            if let Some(item) = self.record_log(item_result) {
                items.push(item);
            }
        }

        items
    }

    pub fn parse_single_item(&mut self) -> ParserResult<Item> {
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
            return Err(ParserLog::ExpectedItem);
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
                // fix: エラー付きで値を返せるようにする
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
                _ => return Err(ParserLog::ExpectedType),
            };

            let kind = TypeKind::Prim(prim_type);
            let r#type = Type { kind: Box::new(kind) };
            Ok(r#type)
        } else {
            Err(ParserLog::ExpectedType)
        }
    }

    pub fn parse_expr(&mut self) -> ParserResult<Expr> {
        if self.consume_keyword(Keyword::Let) {
            self.parse_var_decl_or_init_expr()
        } else {
            Err(ParserLog::ExpectedExpr)
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
                return Err(ParserLog::ExpectedToken);
            }
        };

        Ok(expr)
    }
}
