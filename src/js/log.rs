use crate::log::*;
use crate::lexer::token::Span;

pub type JsifyResult<T> = Result<T, JsifyLog>;

#[derive(Clone, Debug, PartialEq)]
pub enum JsifyLog {
    UnknownSysEmbedName { name: String, span: Span },
}

impl From<JsifyLog> for CompilerLog {
    fn from(value: JsifyLog) -> Self {
        match value {
            JsifyLog::UnknownSysEmbedName { name, span } => CompilerLog::err(
                CompilerErr::UnknownSysEmbedName { name },
                span,
            ),
        }
    }
}
