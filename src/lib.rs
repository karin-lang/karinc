pub mod hir;
pub mod jsir;
pub mod lexer;
pub mod parser;
pub mod tir;
#[cfg(test)]
mod tests;

pub struct JsCompiler;
