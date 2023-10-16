use parsely_lexer::{
    tokens::{self, Token},
    Tok,
};

use crate::{Brackets, Parse, ParseError, ParseStream, Punctuation, Result};

#[derive(Debug, Clone)]
pub enum Literal {
    Int(LiteralInt),
    Float(LiteralFloat),
    String(LiteralString),
}

impl Parse for Literal {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Int(_) => Ok(Literal::Int(stream.parse()?)),
            Token::Float(_) => Ok(Literal::Float(stream.parse()?)),
            Token::String(_) => Ok(Literal::String(stream.parse()?)),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::Int::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralInt {
    pub value: tokens::Int,
}

impl Parse for LiteralInt {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Int(i) => Ok(LiteralInt {
                value: stream.next_ref(i),
            }),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::Int::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralFloat {
    pub value: tokens::Float,
}

impl Parse for LiteralFloat {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Float(i) => Ok(LiteralFloat {
                value: stream.next_ref(i),
            }),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::Int::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralString {
    pub value: tokens::String,
}

impl Parse for LiteralString {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::String(i) => Ok(LiteralString {
                value: stream.next_ref(i),
            }),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::Int::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub dot_tok: Tok![.],
    pub ident: tokens::Ident,
}

impl Parse for Enum {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        Ok(Enum {
            dot_tok: stream.parse()?,
            ident: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Ident(tokens::Ident),
    Enum(Enum),
    ArrayInit(ArrayInit),
    Template(tokens::Template),
}

impl Parse for Expression {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        match stream.peek()? {
            Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Bracket,
                ..
            }) => stream.parse().map(|array| Expression::ArrayInit(array)),
            Token::Ident(ident) => Ok(Expression::Ident(stream.next_ref(ident))),
            Token::Template(t) => Ok(Expression::Template(stream.next_ref(t))),
            Token::Dot(_) => stream.parse().map(Expression::Enum),
            _ => stream.parse().map(|tok| Expression::Literal(tok)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayInit {
    pub elements: Brackets<Punctuation<Expression, tokens::Tok![,]>>,
}

impl ArrayInit {
    pub fn dimensions(&self) -> Vec<usize> {
        let mut v = vec![self.elements.value.len()];

        for f in self.elements.value.iter() {
            match f {
                Expression::ArrayInit(a) => {
                    v.extend(a.dimensions());
                    break;
                }
                _ => (),
            }
        }

        v
    }
}

impl Parse for ArrayInit {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        Ok(ArrayInit {
            elements: stream.parse()?,
        })
    }
}

#[cfg(test)]
mod test {}
