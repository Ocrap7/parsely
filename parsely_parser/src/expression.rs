use parsely_lexer::{
    tokens::{self, Token},
    AsSpan, Tok,
};

use crate::{Brackets, Parse, ParseError, ParseStream, Punctuation, Result};

#[derive(Debug, Clone)]
pub enum Literal {
    Int(LiteralInt),
    Float(LiteralFloat),
    String(LiteralString),
}

impl AsSpan for Literal {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Literal::Int(s) => s.value.as_span(),
            Literal::Float(s) => s.value.as_span(),
            Literal::String(s) => s.value.as_span(),
        }
    }
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

impl AsSpan for Enum {
    fn as_span(&self) -> parsely_lexer::Span {
        self.dot_tok.as_span().join(self.ident.as_span())
    }
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

impl AsSpan for Expression {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Expression::Literal(lit) => lit.as_span(),
            Expression::Ident(ident) => ident.as_span(),
            Expression::Enum(enu) => enu.as_span(),
            Expression::ArrayInit(arr) => arr.as_span(),
            Expression::Template(templ) => templ.as_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayInit {
    pub elements: Brackets<Punctuation<Expression, tokens::Tok![,]>>,
}

impl AsSpan for ArrayInit {
    fn as_span(&self) -> parsely_lexer::Span {
        self.elements.parens.span
    }
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
