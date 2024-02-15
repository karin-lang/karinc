use std::{iter::Peekable, slice::Iter};
use crate::new::data::{ast::*, token::*};

#[macro_export]
macro_rules! seq {
    (name: $name:expr; input: $input:expr; $($result:expr $(=> $reflect:expr)?;)*) => {
        'seq: {
            use crate::new::{data::ast::*, parser::*};

            let start_input = $input.clone();
            #[allow(unused)]
            let mut children = Vec::new();

            $(
                match $result {
                    #[allow(unused)]
                    ParserCombinatoryResult::Matched(child) => {
                        $(
                            if $reflect {
                                children.push(child)
                            }
                        )?
                    },
                    ParserCombinatoryResult::Unmatched => {
                        $input = start_input;
                        break 'seq ParserCombinatoryResult::Unmatched
                    },
                }
            )*

            let node = AstNode::new($name.to_string(), children);
            ParserCombinatoryResult::Matched(AstChild::Node(node))
        }
    };
}

#[macro_export]
macro_rules! choice {
    (input: $input:expr; $($result:expr;)*) => {
        'choice: {
            use crate::new::parser::*;

            $(
                if let ParserCombinatoryResult::Matched(child) = $result {
                    break 'choice ParserCombinatoryResult::Matched(child);
                };
            )*

            ParserCombinatoryResult::Unmatched
        }
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParserResult<T> {
    Matched(T),
    Unmatched,
}

pub type ParserCombinatoryResult = ParserResult<AstChild>;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserLog {}

pub struct Parser {
    pub(crate) logs: Vec<ParserLog>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            logs: Vec::new(),
        }
    }

    pub fn parse(mut self, input: &Vec<Token>) -> ParserResult<(Ast, Vec<ParserLog>)> {
        let input = &mut input.iter().peekable();

        let result = choice!(
            input: *input;
            self.parse_any_symbol(input);
            seq!(
                name: "root";
                input: *input;
                self.parse_any_id(input) => true;
                self.parse_any_id(input) => true;
            );
        );

        let root = match result {
            ParserCombinatoryResult::Matched(child) => match child {
                AstChild::Node(node) => node,
                AstChild::Leaf(leaf) => AstNode::new("root".to_string(), vec![AstChild::Leaf(leaf)]),
            },
            ParserCombinatoryResult::Unmatched => return ParserResult::Unmatched,
        };

        ParserResult::Matched((Ast::new(root), self.logs))
    }

    pub fn parse_any_id(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        match input.peek().cloned().cloned() {
            Some(token) => {
                if let TokenKind::Identifier(_) = token.kind {
                    input.next();
                    ParserCombinatoryResult::Matched(AstChild::Leaf(AstLeaf::new(token)))
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    }

    pub fn parse_keyword(&mut self, input: &mut Peekable<Iter<Token>>, keyword: KeywordToken) -> ParserCombinatoryResult {
        match input.peek().cloned().cloned() {
            Some(token) => {
                match &token.kind {
                    TokenKind::Keyword(next_keyword) if keyword == *next_keyword => {
                        input.next();
                        ParserCombinatoryResult::Matched(AstChild::Leaf(AstLeaf::new(token)))
                    },
                    _ => ParserCombinatoryResult::Unmatched,
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    }

    pub fn parse_symbol(&mut self, input: &mut Peekable<Iter<Token>>, symbol: SymbolToken) -> ParserCombinatoryResult {
        match input.peek().cloned().cloned() {
            Some(token) => {
                match &token.kind {
                    TokenKind::Symbol(next_symbol) if symbol == *next_symbol => {
                        input.next();
                        ParserCombinatoryResult::Matched(AstChild::Leaf(AstLeaf::new(token)))
                    },
                    _ => ParserCombinatoryResult::Unmatched,
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    }

    pub fn parse_any_symbol(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        match input.peek().cloned().cloned() {
            Some(token) => {
                if let TokenKind::Symbol(_) = token.kind {
                    input.next();
                    ParserCombinatoryResult::Matched(AstChild::Leaf(AstLeaf::new(token)))
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    }
}
