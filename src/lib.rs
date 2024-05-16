pub mod hir;
// pub mod jsir;
pub mod lexer;
pub mod parser;
pub mod typesys;
#[cfg(test)]
mod tests;

pub struct JsCompiler;
