use std::{iter::Peekable, slice::Iter};
use crate::{ir::hir::*, lexer::token::*};

macro_rules! next {
    ($input:expr) => {
        $input.next()
    };
}

macro_rules! next_or_exit {
    ($input:expr) => {
        match $input.next() {
            Some(v) => v,
            None => return ParserCombinatoryResult::Unmatched,
        }
    };

    ($input:expr, $token:expr) => {
        match $input.next() {
            Some(v) if v.1 == $token => v,
            _ => return ParserCombinatoryResult::Unmatched,
        }
    };
}

macro_rules! peek_or_exit {
    ($input:expr) => {
        match $input.peek() {
            Some(v) => v,
            None => return ParserCombinatoryResult::Unmatched,
        }
    };

    ($input:expr, $token:expr) => {
        match $input.peek() {
            Some(v) if v.1 == $token => v,
            _ => return ParserCombinatoryResult::Unmatched,
        }
    };
}

pub type ParserResult = (Hir, Vec<ParserLog>);

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {
    ExpectedItemDeclarationOrUseStatement(TokenPosition),
    ExpectedIdentifier(TokenPosition),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParserCombinatoryResult<T> {
    Matched(T),
    Unmatched,
    Ignored,
}

pub struct Parser<'a> {
    input: Peekable<Iter<'a, (TokenPosition, Token)>>,
    logs: Vec<ParserLog>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &Vec<(TokenPosition, Token)>) -> Parser {
        Parser {
            input: input.iter().peekable(),
            logs: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParserResult {
        let mut items = Vec::new();

        loop {
            let start_token_position = match self.input.peek() {
                Some(v) => v.0,
                None => break,
            };

            match self.parse_item() {
                ParserCombinatoryResult::Matched(v) => {
                    items.push(v);
                    continue;
                },
                ParserCombinatoryResult::Unmatched => (),
                ParserCombinatoryResult::Ignored => continue,
            }

            self.logs.push(ParserLog::ExpectedItemDeclarationOrUseStatement(start_token_position));
            self.input.next();
        }

        let hir = Hir { items };
        (hir, self.logs)
    }

    pub fn parse_item(&mut self) -> ParserCombinatoryResult<HirItem> {
        match self.parse_function() {
            ParserCombinatoryResult::Matched(v) => return ParserCombinatoryResult::Matched(HirItem::Function(v)),
            ParserCombinatoryResult::Unmatched => (),
            ParserCombinatoryResult::Ignored => return ParserCombinatoryResult::Ignored,
        };

        ParserCombinatoryResult::Unmatched
    }

    fn parse_function(&mut self) -> ParserCombinatoryResult<HirFunction> {
        let mut current_input = self.input.clone();

        next_or_exit!(current_input, Token::Keyword(KeywordToken::Function));

        let id = {
            let id_token = peek_or_exit!(current_input);

            match &id_token.1 {
                Token::Identifier(v) => {
                    next!(current_input);
                    Some(v.clone())
                },
                _ => {
                    self.logs.push(ParserLog::ExpectedIdentifier(id_token.0));
                    None
                },
            }
        };

        next_or_exit!(current_input, Token::Symbol(SymbolToken::OpenParen));
        next_or_exit!(current_input, Token::Symbol(SymbolToken::ClosingParen));
        next_or_exit!(current_input, Token::Symbol(SymbolToken::OpenCurlyBracket));
        next_or_exit!(current_input, Token::Symbol(SymbolToken::ClosingCurlyBracket));

        self.input = current_input;

        if let Some(id) = id {
            ParserCombinatoryResult::Matched(HirFunction { id })
        } else {
            ParserCombinatoryResult::Ignored
        }
    }
}
