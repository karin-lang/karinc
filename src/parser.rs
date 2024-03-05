pub mod ast;

use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::token::*;
use self::ast::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {
    ExpectedToken,
    UnexpectedEof,
}

pub type ParserResult<T> = Result<T, ParserLog>;

// todo: シンボル管理を実装する
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

    pub fn next_line(&mut self) {
        // fix: 次のトークンを次の改行に修正する（トークン位置の情報を基準にする）
        self.tokens.next();
    }

    pub fn consume_if_id(&mut self) -> Option<Id> {
        if let Some(token) = self.tokens.peek() {
            if let TokenKind::Id(id) = &token.kind {
                self.tokens.next();
                return Some(id.into());
            }
        }

        None
    }

    pub fn consume_if_keyword(&mut self, keyword: KeywordToken) -> bool {
        self
            .tokens
            .next_if(|next_token: &&Token| next_token.kind == TokenKind::Keyword(keyword))
            .is_some()
    }

    pub fn consume_if_symbol(&mut self, symbol: SymbolToken) -> bool {
        self
            .tokens
            .next_if(|next_token: &&Token| next_token.kind == TokenKind::Symbol(symbol))
            .is_some()
    }

    pub fn expect_id(&mut self) -> ParserResult<Id> {
        self.consume_if_id().ok_or(ParserLog::ExpectedToken)
    }

    pub fn expect_symbol(&mut self, symbol: SymbolToken) -> ParserResult<()> {
        if self.consume_if_symbol(symbol) {
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
        let kind = if self.consume_if_keyword(KeywordToken::Function) {
            let id = self.expect_id()?;
            let args = self.parse_formal_args()?;
            let body = self.parse_body()?;
            let decl = FnDecl { id, args, body };
            ItemKind::FnDecl(decl)
        } else {
            // todo: もっといいトークンの進め先を考える
            self.next_line();
            return Err(ParserLog::ExpectedToken);
        };

        let item = Item { kind };
        Ok(item)
    }

    pub fn parse_formal_args(&mut self) -> ParserResult<Vec<FormalArg>> {
        self.expect_symbol(SymbolToken::OpenParen)?;
        let args = Vec::new();

        // todo: 修正
        loop {
            if self.is_eof() {
                break;
            }

            if self.consume_if_symbol(SymbolToken::ClosingParen) {
                break;
            }

            self.tokens.next();
        }

        Ok(args)
    }

    pub fn parse_body(&mut self) -> ParserResult<Vec<Expr>> {
        self.expect_symbol(SymbolToken::OpenCurlyBracket)?;
        let exprs = Vec::new();

        // todo: 実装
        loop {
            if self.is_eof() {
                break;
            }

            if self.consume_if_symbol(SymbolToken::ClosingCurlyBracket) {
                break;
            }

            self.tokens.next();
        }

        Ok(exprs)
    }
}
