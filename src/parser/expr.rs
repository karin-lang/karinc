use crate::{*, parser::*};

impl Parser {
    pub fn parse_expr(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        choice!(
            input: *input;
            self.parse_number_literal(input);
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
            self.parse_any_id(input) => Visible;
            self.parse_symbol(input, SymbolToken::OpenParen);
            self.parse_actual_function_args(input) => Visible;
            self.parse_symbol(input, SymbolToken::ClosingParen);
        )
    }

    pub fn parse_actual_function_args(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        optional!(
            seq!(
                name: "actual_fn_args";
                input: *input;
                self.parse_expr(input) => Visible;
                min!(
                    min: 0;
                    name: "";
                    input: *input;
                    seq!(
                        name: "";
                        input: *input;
                        self.parse_symbol(input, SymbolToken::Comma);
                        self.parse_expr(input) => Visible;
                    );
                ) => Expanded;
                optional!(self.parse_symbol(input, SymbolToken::Comma));
            );
        )
    }
}
