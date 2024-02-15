use crate::{*, new::parser::*};

impl Parser {
    pub fn parse_expr(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        choice!(
            input: *input;
            self.parse_any_number(input);
            self.parse_function_call(input);
        )
    }

    pub fn parse_number_literal(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        self.parse_any_number(input)
    }

    pub fn parse_function_call(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        seq!(
            name: "fn_call";
            input: *input;
            self.parse_any_id(input) => true;
            self.parse_symbol(input, SymbolToken::OpenParen);
            self.parse_symbol(input, SymbolToken::ClosingParen);
        )
    }
}
