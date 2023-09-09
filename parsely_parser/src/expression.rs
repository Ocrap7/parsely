use parsely_lexer::{
    tokens::{self, Group, GroupBracket, Token},
    AsSpan,
};

use crate::{Brackets, Parens, Parse, ParseError, ParseStream, Punctuation, Result};

#[derive(Debug, Clone)]
pub enum Literal {
    Int(LiteralInt),
    Float(LiteralFloat),
    String(LiteralString),
}

impl AsSpan for Literal {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Literal::Int(l) => l.value.as_span(),
            Literal::Float(l) => l.value.as_span(),
            Literal::String(l) => l.value.as_span(),
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
pub enum Expression {
    Literal(Literal),
    Ident(tokens::Ident),
    ArrayInit(ArrayInit),
    Parens(Parens<Expression>),
    BinOp(BinOp),
}

impl AsSpan for Expression {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Expression::Literal(l) => l.as_span(),
            Expression::BinOp(b) => b.as_span(),
            Expression::Ident(i) => i.as_span(),
            Expression::Parens(i) => i.value.as_span(),
            Expression::ArrayInit(i) => i.as_span(),
        }
    }
}

impl Expression {
    fn parse_primary_expression(stream: &'_ ParseStream<'_>) -> Result<Expression> {
        match stream.peek()? {
            Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Paren,
                ..
            }) => stream.parse().map(|parens| Expression::Parens(parens)),
            Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Bracket,
                ..
            }) => stream.parse().map(|array| Expression::ArrayInit(array)),
            Token::Ident(ident) => Ok(Expression::Ident(stream.next_ref(ident))),
            _ => stream.parse().map(|tok| Expression::Literal(tok)),
        }
    }
}

impl Parse for Expression {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        BinOp::parse_binop(stream, 0)
    }
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub left: Box<Expression>,
    pub op: tokens::Token,
    pub right: Box<Expression>,
}

impl AsSpan for BinOp {
    fn as_span(&self) -> parsely_lexer::Span {
        self.left.as_span().join(self.right.as_span())
    }
}

impl BinOp {
    fn parse_binop(stream: &'_ ParseStream<'_>, last_prec: usize) -> Result<Expression> {
        let mut left = Expression::parse_primary_expression(stream)?;

        while stream.has_next() {
            let op = stream.peek()?;

            let prec = BinOp::precedence(&op);

            if prec < last_prec || prec == 0 {
                break;
            }

            let op = stream.next();

            let right = BinOp::parse_binop(stream, prec)?;

            left = Expression::BinOp(BinOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn precedence(op: &tokens::Token) -> usize {
        match op {
            tokens::Tok![enum *] | tokens::Tok![enum /] | tokens::Tok![enum %] => 130,
            tokens::Tok![enum +] | tokens::Tok![enum -] => 120,
            tokens::Tok![enum <<] | tokens::Tok![enum >>] => 110,
            tokens::Tok![enum <]
            | tokens::Tok![enum >]
            | tokens::Tok![enum <=]
            | tokens::Tok![enum >=] => 100,
            tokens::Tok![enum ==] | tokens::Tok![enum !=] => 90,
            tokens::Tok![enum &] => 80,
            tokens::Tok![enum ^] => 70,
            tokens::Tok![enum |] => 60,
            tokens::Tok![enum &&] => 50,
            tokens::Tok![enum ||] => 40,
            tokens::Tok![enum =]
            | tokens::Tok![enum +=]
            | tokens::Tok![enum -=]
            | tokens::Tok![enum *=]
            | tokens::Tok![enum /=]
            | tokens::Tok![enum %=]
            | tokens::Tok![enum <<=]
            | tokens::Tok![enum >>=]
            | tokens::Tok![enum &=]
            | tokens::Tok![enum ^=]
            | tokens::Tok![enum |=] => 30,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayInit {
    pub elements: Brackets<Punctuation<Expression, tokens::Tok![,]>>,
}

impl AsSpan for ArrayInit {
    fn as_span(&self) -> parsely_lexer::Span {
        self.elements.value.as_span()
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

#[derive(Debug, Clone)]
pub struct Call {
    pub expr: Box<Expression>,
    pub args: Parens<Punctuation<Expression, tokens::Tok![,]>>,
}

#[cfg(test)]
mod test {}
