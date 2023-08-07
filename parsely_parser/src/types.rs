use parsely_lexer::tokens::{self, Token};
use parsely_macros::AsSpan;

use crate::{Parse, ParseError};

#[derive(Debug, Clone, AsSpan)]
pub struct ArrayType {
    pub array_tok: tokens::ArrayTy,
    pub of_tok: tokens::Of,
    pub base_type: Box<Type>,
}

impl Parse for ArrayType {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ArrayType {
            array_tok: stream.parse()?,
            of_tok: stream.parse()?,
            base_type: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum Type {
    Int(tokens::IntTy),
    Float(tokens::FloatTy),
    Bool(tokens::BoolTy),
    Named(tokens::Ident),
    Array(ArrayType),
}

impl Parse for Type {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum i @ integer:noun] => Ok(Type::Int(stream.next_ref(i))),
            tokens::Tok![enum i @ float:noun] => Ok(Type::Float(stream.next_ref(i))),
            tokens::Tok![enum i @ boolean:noun] => Ok(Type::Bool(stream.next_ref(i))),
            tokens::Tok![enum array:noun] => stream.parse().map(Type::Array),
            Token::Ident(i) => Ok(Type::Named(stream.next_ref(i))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "type".into(),
            }),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct OfType {
    pub of_tok: tokens::Of,
    pub type_tok: tokens::Type,
    pub ty: Type,
}

impl Parse for OfType {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(OfType {
            of_tok: stream.parse()?,
            type_tok: stream.parse()?,
            ty: stream.parse()?,
        })
    }
}
