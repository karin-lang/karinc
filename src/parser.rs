use std::{iter::Peekable, slice::Iter};
use crate::{ir::hir::*, lexer::token::*};

macro_rules! seq {
    ($($result:expr)*) => {
        {
            $(
                match $result {
                    ParserCombinatoryResult::Matched(_) => (),
                    ParserCombinatoryResult::Unmatched => return ParserCombinatoryResult::Unmatched,
                }
            )*

            ParserCombinatoryResult::Matched(())
        }
    };
}

macro_rules! choice {
    ($($result:expr)*) => {
        'choice: {
            $(
                if let ParserCombinatoryResult::Matched(_) = $result {
                    break 'choice ParserCombinatoryResult::Matched(());
                };
            )*

            ParserCombinatoryResult::Unmatched
        }
    };
}

macro_rules! id {
    ($input:expr) => {
        match $input.peek() {
            Some((pos, token)) => {
                if let Token::Identifier(id) = token {
                    $input.next();
                    ParserCombinatoryResult::Matched((pos, id))
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    };
}

macro_rules! keyword {
    ($input:expr) => {
        match $input.peek() {
            Some((pos, token)) => {
                if let Token::Keyword(keyword) = token {
                    $input.next();
                    ParserCombinatoryResult::Matched((pos, keyword))
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    };

    ($input:expr, $keyword:ident) => {
        match $input.peek() {
            Some((pos, token)) if *token == Token::Keyword(KeywordToken::$keyword) => {
                $input.next();
                ParserCombinatoryResult::Matched((pos, KeywordToken::$keyword))
            },
            _ => ParserCombinatoryResult::Unmatched,
        }
    };
}

macro_rules! symbol {
    ($input:expr) => {
        match $input.peek() {
            Some((pos, token)) => {
                if let Token::Symbol(symbol) = token {
                    $input.next();
                    ParserCombinatoryResult::Matched((pos, symbol))
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
        }
    };

    ($input:expr, $symbol:ident) => {
        match $input.peek() {
            Some((pos, token)) if *token == Token::Symbol(SymbolToken::$symbol) => {
                $input.next();
                ParserCombinatoryResult::Matched((pos, SymbolToken::$symbol))
            },
            _ => ParserCombinatoryResult::Unmatched,
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
}

impl<T> ParserCombinatoryResult<T> {
    pub fn is_matched(&self) -> bool {
        if let ParserCombinatoryResult::Matched(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unmatched(&self) -> bool {
        if let ParserCombinatoryResult::Unmatched = self {
            true
        } else {
            false
        }
    }

    pub fn to_unit(&self) -> ParserCombinatoryResult<()> {
        match self {
            ParserCombinatoryResult::Matched(_) => ParserCombinatoryResult::Matched(()),
            ParserCombinatoryResult::Unmatched => ParserCombinatoryResult::Unmatched,
        }
    }

    pub fn map<O, F>(self, f: F) -> ParserCombinatoryResult<O>
    where
        F: FnOnce(T) -> O,
    {
        match self {
            ParserCombinatoryResult::Matched(v) => ParserCombinatoryResult::Matched(f(v)),
            ParserCombinatoryResult::Unmatched => ParserCombinatoryResult::Unmatched,
        }
    }
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

            let result = choice!(
                {
                    let result = self.parse_item();
                    let unit_result = result.to_unit();

                    if let ParserCombinatoryResult::Matched(Some(new_item)) = result {
                        items.push(new_item);
                    }

                    unit_result
                }
            );

            if let ParserCombinatoryResult::Unmatched = result {
                self.logs.push(ParserLog::ExpectedItemDeclarationOrUseStatement(start_token_position));
                self.input.next();
            }
        }

        let hir = Hir { items };
        (hir, self.logs)
    }

    pub fn parse_item(&mut self) -> ParserCombinatoryResult<Option<HirItem>> {
        let result = self.parse_function();
        result.map(|option| option.map(|f| HirItem::Function(f)))
    }

    pub fn parse_function(&mut self) -> ParserCombinatoryResult<Option<HirFunction>> {
        let mut i = self.input.clone();
        let mut id = None;

        let result = seq!(
            keyword!(i, Function)
            {
                let result = id!(i);

                match result {
                    ParserCombinatoryResult::Matched((_, token)) => id = Some(token.clone()),
                    _ => {
                        let pos = i.peek().map(|v| v.0).unwrap_or(TokenPosition::default());
                        self.logs.push(ParserLog::ExpectedIdentifier(pos))
                    },
                }

                ParserCombinatoryResult::Matched(())
            }
            symbol!(i, OpenParen)
            symbol!(i, ClosingParen)
            symbol!(i, OpenCurlyBracket)
            symbol!(i, ClosingCurlyBracket)
        );

        let id = if let Some(v) = id {
            v
        } else {
            self.input = i;
            return ParserCombinatoryResult::Matched(None);
        };

        result.map(|_| {
            self.input = i;

            let f = HirFunction {
                id: HirIdentifier(id),
            };

            Some(f)
        })
    }

    pub fn parse_identifier(&mut self) -> ParserCombinatoryResult<HirIdentifier> {
        let mut i = self.input.clone();
        let result = id!(i);

        result.map(|(_, v)| {
            self.input = i;
            HirIdentifier(v.clone())
        })
    }
}
