use parsely_lexer::tokens::{self, Token};

use crate::{Parse, ParseError, ParseStream, Result};

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
pub enum Expression {
    Literal(Literal),
    Parens(crate::Parens<Expression>),
    BinOp(BinOp),
}

impl Expression {
    fn parse_primary_expression(stream: &'_ ParseStream<'_>) -> Result<Expression> {
        match stream.peek()? {
            Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Paren,
                ..
            }) => stream.parse().map(|parens| Expression::Parens(parens)),
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

#[cfg(test)]
mod test {
    use parsely_lexer::Lexer;

    use super::*;

    #[test]
    fn test_basic() {
        let input = "4 + 8 * 6 * 3";
        let tokens = Lexer::run(input.as_bytes());
        let stream = ParseStream::from(&tokens);

        let expr: Expression = stream.parse().expect("Parse error!");

        // println!("{:#?}", expr);
    }

    #[test]
    fn test_prec() {
        let input = "4 + 8 * 6 / 2 % 3 - 7 + (3 + 4) * 2";
        let tokens = Lexer::run(input.as_bytes());
        let stream = ParseStream::from(&tokens);

        let expr: Expression = stream.parse().expect("Parse error!");

        // println!("{:#?}", expr);
    }
}