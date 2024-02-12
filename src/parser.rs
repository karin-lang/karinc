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

macro_rules! number {
    ($input:expr) => {
        match $input.peek() {
            Some((pos, token)) => {
                if let Token::Number(number) = token {
                    $input.next();
                    ParserCombinatoryResult::Matched((pos, number))
                } else {
                    ParserCombinatoryResult::Unmatched
                }
            },
            None => ParserCombinatoryResult::Unmatched,
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
    ExpectedSemicolon(TokenPosition),
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

pub type ParserInput<'a> = Peekable<Iter<'a, (TokenPosition, Token)>>;

pub struct Parser {
    pub(crate) logs: Vec<ParserLog>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            logs: Vec::new(),
        }
    }

    pub fn parse(mut self, input: &Vec<(TokenPosition, Token)>) -> ParserResult {
        let mut items = Vec::new();
        let input = &mut input.iter().peekable();

        loop {
            let start_token_position = match input.peek() {
                Some(v) => v.0,
                None => break,
            };

            let result = choice!(
                {
                    let result = self.parse_item_definition(input);
                    let unit_result = result.to_unit();

                    if let ParserCombinatoryResult::Matched(Some(new_item)) = result {
                        items.push(new_item);
                    }

                    unit_result
                }
            );

            if let ParserCombinatoryResult::Unmatched = result {
                self.logs.push(ParserLog::ExpectedItemDeclarationOrUseStatement(start_token_position));
                input.next();
            }
        }

        let hir = Hir { items };
        (hir, self.logs)
    }

    pub fn parse_item_definition(&mut self, input: &mut ParserInput) -> ParserCombinatoryResult<Option<HirItem>> {
        let result = self.parse_function_definition(input);
        result.map(|option| option.map(|f| HirItem::Function(f)))
    }

    pub fn parse_function_definition(&mut self, input: &mut ParserInput) -> ParserCombinatoryResult<Option<HirFunction>> {
        let mut i = input.clone();
        let mut id = None;

        let result = seq!(
            keyword!(i, Function)
            {
                // todo: parse_identifier() に置換する
                let result = id!(i);

                match result {
                    ParserCombinatoryResult::Matched((_, token)) => id = Some(token.clone()),
                    _ => {
                        // todo: 次の位置を拾う処理を関数化する
                        let pos = i.peek().map(|v| v.0.set_len(1)).unwrap_or(TokenPosition::default());
                        self.logs.push(ParserLog::ExpectedIdentifier(pos))
                    },
                }

                ParserCombinatoryResult::Matched(())
            }
            symbol!(i, OpenParen)
            symbol!(i, ClosingParen)
            symbol!(i, OpenCurlyBracket)
            // メモ
            // {
            //     let result = symbol!(i, Semicolon);
            //     let unit_result = result.to_unit();

            //     if result.is_unmatched() {
            //         let pos = i.peek().map(|v| v.0.set_len(1)).unwrap_or(TokenPosition::default());
            //         self.logs.push(ParserLog::ExpectedSemicolon(pos))
            //     }

            //     unit_result
            // }
            symbol!(i, ClosingCurlyBracket)
        );

        let id = if let Some(v) = id {
            v
        } else {
            *input = i;
            return ParserCombinatoryResult::Matched(None);
        };

        result.map(|_| {
            *input = i;

            let f = HirFunction {
                id: HirIdentifier(id),
            };

            Some(f)
        })
    }

    pub fn parse_expression(&mut self, input: &mut ParserInput) -> ParserCombinatoryResult<Option<HirExpression>> {
        let mut i = input.clone();
        let mut expr = None;

        let result = choice!(
            // todo: unit_result を返すプロセスをマクロ化する
            {
                let result = self.parse_function_call(&mut i).map(|v| Some(HirExpression::FunctionCall(v)));
                let unit_result = result.to_unit();

                if let ParserCombinatoryResult::Matched(v) = result {
                    expr = v;
                }

                unit_result
            }
            {
                let result = self.parse_number(&mut i).map(|v| Some(HirExpression::Number(v)));
                let unit_result = result.to_unit();

                if let ParserCombinatoryResult::Matched(v) = result {
                    expr = v;
                }

                unit_result
            }
            {
                let result = self.parse_identifier(&mut i).map(|v| Some(HirExpression::Identifier(v)));
                let unit_result = result.to_unit();

                if let ParserCombinatoryResult::Matched(v) = result {
                    expr = v;
                }

                unit_result
            }
        );

        result.map(|_| {
            *input = i;
            expr
        })
    }

    pub fn parse_function_call(&mut self, input: &mut ParserInput) -> ParserCombinatoryResult<HirFunctionCall> {
        let mut i = input.clone();
        let mut id = HirIdentifier(String::new());

        let result = seq!(
            {
                let result = self.parse_identifier(&mut i);
                let unit_result = result.to_unit();

                if let ParserCombinatoryResult::Matched(v) = result {
                    id = v;
                }

                unit_result
            }
            symbol!(i, OpenParen)
            symbol!(i, ClosingParen)
        );

        result.map(|_| {
            *input = i;
            HirFunctionCall { id }
        })
    }

    pub fn parse_number(&mut self, input: &mut ParserInput) -> ParserCombinatoryResult<HirNumber> {
        let result = number!(input);
        result.map(|(_, v)| HirNumber(v.clone()))
    }

    pub fn parse_identifier(&mut self, input: &mut ParserInput) -> ParserCombinatoryResult<HirIdentifier> {
        let result = id!(input);
        result.map(|(_, v)| HirIdentifier(v.clone()))
    }
}
