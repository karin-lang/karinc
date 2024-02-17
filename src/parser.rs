pub mod expr;
pub mod item;

use std::{iter::Peekable, slice::Iter};
use crate::data::{ast::*, token::*};

#[macro_export]
macro_rules! seq {
    (name: $name:expr; input: $input:expr; $($result:expr $(=> $visibility:ident)?;)*) => {
        'seq: {
            use crate::{data::ast::*, parser::*};

            let start_input = $input.clone();
            #[allow(unused)]
            let mut children = Vec::new();

            $(
                match $result {
                    #[allow(unused)]
                    ParserCombinatoryResult::Matched(child) => {
                        $(
                            if let Some(child) = child {
                                match AstVisibility::$visibility {
                                    AstVisibility::Visible => children.push(child),
                                    AstVisibility::Expanded => {
                                        let mut new_children = match child {
                                            AstChild::Node(node) => node.children,
                                            AstChild::Leaf(leaf) => vec![AstChild::Leaf(leaf)],
                                        };

                                        children.append(&mut new_children);
                                    },
                                    AstVisibility::Hidden => (),
                                }
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
            ParserCombinatoryResult::Matched(Some(AstChild::Node(node)))
        }
    };
}

#[macro_export]
macro_rules! choice {
    (input: $input:expr; $($result:expr;)*) => {
        'choice: {
            use crate::parser::*;

            $(
                if let ParserCombinatoryResult::Matched(child) = $result {
                    break 'choice ParserCombinatoryResult::Matched(child);
                };
            )*

            ParserCombinatoryResult::Unmatched
        }
    };
}

#[macro_export]
macro_rules! optional {
    ($result:expr $(;)?) => {
        {
            use crate::parser::*;

            let parsed = $result;

            match parsed {
                ParserResult::Unmatched => ParserResult::Matched(None),
                _ => parsed,
            }
        }
    };
}

#[macro_export]
macro_rules! min {
    (min: $min:expr; name: $name:expr; input: $input:expr; $result:expr $(;)?) => {
        {
            use crate::parser::*;

            let start_input = $input.clone();
            let mut children = Vec::new();

            loop {
                match $result {
                    ParserCombinatoryResult::Matched(option) => match option {
                        Some(child) => children.push(child),
                        None => (),
                    },
                    ParserCombinatoryResult::Unmatched => break,
                }
            }

            #[allow(unused_comparisons)]
            if $min <= children.len() {
                let child = AstChild::node($name.to_string(), children);
                ParserCombinatoryResult::Matched(Some(child))
            } else {
                $input = start_input;
                ParserCombinatoryResult::Unmatched
            }
        }
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParserResult<T> {
    Matched(T),
    Unmatched,
}

impl<T> ParserResult<T> {
    pub fn unwrap(self) -> T {
        match self {
            ParserResult::Matched(v) => v,
            ParserResult::Unmatched => panic!("called `ParserResult::<T>::unwrap()` on a `Unmatched` value"),
        }
    }
}

pub type ParserCombinatoryResult = ParserResult<Option<AstChild>>;

impl ParserResult<Option<AstChild>> {
    pub fn rename(self, name: &str) -> Self {
        match self {
            ParserCombinatoryResult::Matched(option) => match option {
                Some(child) => ParserCombinatoryResult::Matched(Some(child.rename(name))),
                None => ParserCombinatoryResult::Matched(None),
            },
            _ => self,
        }
    }
}

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

    pub fn parse(mut self, input: &Vec<Token>) -> (ParserResult<Option<Ast>>, Vec<ParserLog>) {
        let input = &mut input.iter().peekable();

        let result = min!(
            min: 0;
            name: "root";
            input: *input;
            self.parse_item(input);
        );

        if input.peek().is_some() {
            return (ParserResult::Unmatched, self.logs);
        }

        let root = match result {
            ParserCombinatoryResult::Matched(child) => match child {
                Some(child) => match child {
                    AstChild::Node(node) => Some(Ast::new(node).normalize()),
                    AstChild::Leaf(_) => None,
                },
                None => None,
            },
            ParserCombinatoryResult::Unmatched => return (ParserResult::Unmatched, self.logs),
        };

        (ParserResult::Matched(root), self.logs)
    }

    pub fn parse_any_number(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        match input.peek().cloned().cloned() {
            Some(token) => {
                if let TokenKind::Number(_) = token.kind {
                    input.next();

                    ParserCombinatoryResult::Matched(
                        Some(AstChild::Leaf(AstLeaf::new("number".to_string(), token))),
                    )
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    }

    pub fn parse_any_id(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        match input.peek().cloned().cloned() {
            Some(token) => {
                if let TokenKind::Id(_) = token.kind {
                    input.next();

                    ParserCombinatoryResult::Matched(
                        Some(AstChild::Leaf(AstLeaf::new("id".to_string(), token))),
                    )
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

                        ParserCombinatoryResult::Matched(
                            Some(AstChild::Leaf(AstLeaf::new("keyword".to_string(), token))),
                        )
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

                        ParserCombinatoryResult::Matched(
                            Some(AstChild::Leaf(AstLeaf::new("symbol".to_string(), token))),
                        )
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
                    ParserCombinatoryResult::Matched(
                        Some(AstChild::Leaf(AstLeaf::new("symbol".to_string(), token))),
                    )
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    }
}
