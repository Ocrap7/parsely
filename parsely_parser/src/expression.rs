use parsely_lexer::tokens::{self, Token};
use parsely_macros::AsSpan;

use crate::{Parse, ParseError, PunctuationLast};

#[derive(Debug, Clone, AsSpan)]
pub enum Literal {
    Int(tokens::Int),
    Float(tokens::Float),
    Bool(tokens::Bool),
    String(tokens::String),
}

impl Parse for Literal {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Int(i) => Ok(Literal::Int(stream.next_ref(i))),
            Token::Float(i) => Ok(Literal::Float(stream.next_ref(i))),
            Token::Bool(i) => Ok(Literal::Bool(stream.next_ref(i))),
            Token::String(i) => Ok(Literal::String(stream.next_ref(i))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "literal".into(),
            }),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct Value {
    pub the_tok: tokens::The,
    pub value_tok: tokens::Value,
    pub lit: Literal,
}

impl Parse for Value {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Value {
            the_tok: stream.parse()?,
            value_tok: stream.parse()?,
            lit: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct ResultOf {
    pub the_tok: tokens::The,
    pub result: tokens::Result,
    pub of: tokens::Of,
    pub op: OpList,
}

impl Parse for ResultOf {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ResultOf {
            the_tok: stream.parse()?,
            result: stream.parse()?,
            of: stream.parse()?,
            op: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct ValueOf {
    pub the_tok: tokens::The,
    pub value_tok: tokens::Value,
    pub of: tokens::Of,
    pub ident: tokens::Ident,
}

impl Parse for ValueOf {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ValueOf {
            the_tok: stream.parse()?,
            value_tok: stream.parse()?,
            of: stream.parse()?,
            ident: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum Expression {
    Value(Value),
    ValueOf(ValueOf),
    Result(ResultOf),
}

impl Parse for Expression {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peekn(1)? {
            tokens::Tok![enum value] => match stream.peekn(2) {
                Ok(tokens::Tok![enum of]) => stream.parse().map(Expression::ValueOf),
                _ => stream.parse().map(Expression::Value),
            },
            tokens::Tok![enum result] => stream.parse().map(Expression::Result),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "expression".into(),
            }),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum BinOperator {
    Add(tokens::Adding),
    Sub(tokens::Subtracting),
    Mult(tokens::Multiplying),
    Div(tokens::Dividing),
}

impl Parse for BinOperator {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Adding(a) => Ok(BinOperator::Add(stream.next_ref(a))),
            Token::Subtracting(a) => Ok(BinOperator::Sub(stream.next_ref(a))),
            Token::Multiplying(a) => Ok(BinOperator::Mult(stream.next_ref(a))),
            Token::Dividing(a) => Ok(BinOperator::Div(stream.next_ref(a))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "binary operator".into(),
            }),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum UnaryOperator {
    Neg(tokens::Negating),
}

impl Parse for UnaryOperator {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Negating(n) => Ok(UnaryOperator::Neg(stream.next_ref(n))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "unary operator".into(),
            }),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct BinOp {
    pub op: BinOperator,
    pub left: Box<Expression>,
    pub and_tok: tokens::And,
    pub right: Box<Expression>,
}

impl Parse for BinOp {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(BinOp {
            op: stream.parse()?,
            left: stream.parse()?,
            and_tok: stream.parse()?,
            right: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct UnaryOp {
    pub op: UnaryOperator,
    pub expr: Box<Expression>,
}

impl Parse for UnaryOp {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(UnaryOp {
            op: stream.parse()?,
            expr: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct OpBy {
    pub op: BinOperator,
    pub by: tokens::By,
    pub expr: Box<Expression>,
}

impl Parse for OpBy {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(OpBy {
            op: stream.parse()?,
            by: stream.parse()?,
            expr: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct UnaryImpl {
    pub op: UnaryOperator,
}

impl Parse for UnaryImpl {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(UnaryImpl {
            op: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum Operation {
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    OpBy(OpBy),
    UnaryImpl(UnaryImpl),
}

#[derive(Debug, Clone, AsSpan)]
pub enum BinOrUnary {
    BinOp(BinOp),
    UnaryOp(UnaryOp),
}

impl Parse for BinOrUnary {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        if let Ok(op) = stream.parse() {
            Ok(BinOrUnary::BinOp(BinOp {
                op,
                left: stream.parse()?,
                and_tok: stream.parse()?,
                right: stream.parse()?,
            }))
        } else if let Ok(op) = stream.parse() {
            Ok(BinOrUnary::UnaryOp(UnaryOp {
                op,
                expr: stream.parse()?,
            }))
        } else {
            Err(ParseError::UnexpectedToken {
                found: stream.peek()?.clone(),
                expected: "operator".into(),
            })
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum ByOrImpl {
    OpBy(OpBy),
    UnaryImpl(UnaryImpl),
}

impl Parse for ByOrImpl {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        if let Ok(op) = stream.parse() {
            Ok(ByOrImpl::OpBy(OpBy {
                op,
                by: stream.parse()?,
                expr: stream.parse()?,
            }))
        } else if let Ok(op) = stream.parse() {
            Ok(ByOrImpl::UnaryImpl(UnaryImpl { op }))
        } else {
            Err(ParseError::UnexpectedToken {
                found: stream.peek()?.clone(),
                expected: "operator".into(),
            })
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct OpList {
    pub first: Option<BinOrUnary>,
    pub comma: Option<tokens::Tok![,]>,
    pub rest: PunctuationLast<ByOrImpl, tokens::Tok![,], tokens::Tok![and]>,
}

impl OpList {
    pub fn iter_operation(&self) -> impl Iterator<Item = Operation> + '_ {
        self.first
            .as_ref()
            .map(|op| match op {
                BinOrUnary::BinOp(bin) => Operation::BinOp(bin.clone()),
                BinOrUnary::UnaryOp(unary) => Operation::UnaryOp(unary.clone()),
            })
            .into_iter()
            .chain(self.rest.iter().map(|op| match op {
                ByOrImpl::OpBy(onby) => Operation::OpBy(onby.clone()),
                ByOrImpl::UnaryImpl(un) => Operation::UnaryImpl(un.clone()),
            }))
    }
}

impl Parse for OpList {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let first = stream.parse().ok();

        match stream.peek() {
            Ok(Token::Comma(c)) if first.is_some() => {
                let comma = stream.next_ref(c);
                let rest: PunctuationLast<ByOrImpl, tokens::Tok![,], tokens::Tok![and]> =
                    stream.parse()?;

                if rest.is_empty() {
                    return Err(ParseError::UnexpectedToken {
                        found: Token::Comma(comma.clone()),
                        expected: "operations".into(),
                    });
                }

                Ok(OpList {
                    first,
                    comma: Some(comma),
                    rest,
                })
            }
            _ => Ok(OpList {
                first,
                comma: None,
                rest: PunctuationLast::empty(),
            }),
        }
    }
}
