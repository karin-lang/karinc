use crate::{*, new::parser::*};

impl Parser {
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
            Parser::optional(
                self.parse_accessibility(input),
            ) => true;
            self.parse_keyword(input, KeywordToken::Function);
            self.parse_any_id(input) => true;
            self.parse_symbol(input, SymbolToken::OpenParen);
            self.parse_symbol(input, SymbolToken::ClosingParen);
            self.parse_symbol(input, SymbolToken::OpenCurlyBracket);
            self.parse_symbol(input, SymbolToken::ClosingCurlyBracket);
        )
    }
}
