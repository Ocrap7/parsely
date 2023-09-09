use parsely_lexer::{Tok, tokens, AsSpan};

use crate::Parse;

#[derive(Debug, Clone)]
pub enum Type {
    Bool(tokens::Bool),
    Int(tokens::Int),
}

impl Parse for Type {
    fn parse(stream: &'_ mut crate::ParseStream) -> parsely_diagnostics::Result<Self> {
        todo!()
    }
}

impl AsSpan for Type {
    fn as_span(&self) -> parsely_lexer::Span {
        todo!()
    }
}