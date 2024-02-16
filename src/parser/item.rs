use crate::{*, parser::*};

impl Parser {
    pub fn parse_item(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        choice!(
            input: *input;
            self.parse_function_declaration(input);
        )
    }

    pub fn parse_accessibility(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        choice!(
            input: *input;
            self.parse_keyword(input, KeywordToken::Public);
        ).rename("accessibility")
    }

    pub fn parse_function_declaration(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        seq!(
            name: "fn_dec";
            input: *input;
            optional!(self.parse_accessibility(input)) => Visible;
            self.parse_keyword(input, KeywordToken::Function);
            self.parse_any_id(input) => Visible;
            self.parse_symbol(input, SymbolToken::OpenParen);
            self.parse_symbol(input, SymbolToken::ClosingParen);
            self.parse_symbol(input, SymbolToken::OpenCurlyBracket);
            self.parse_function_exprs(input) => Visible;
            self.parse_symbol(input, SymbolToken::ClosingCurlyBracket);
        )
    }

    pub fn parse_function_exprs(&mut self, input: &mut Peekable<Iter<Token>>) -> ParserCombinatoryResult {
        min!(
            min: 0;
            name: "fn_exprs";
            input: *input;
            seq!(
                name: "";
                input: *input;
                self.parse_expr(input) => Visible;
                self.parse_symbol(input, SymbolToken::Semicolon);
            );
        )
    }
}
