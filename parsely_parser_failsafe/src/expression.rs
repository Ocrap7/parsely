use parsely_diagnostics::{Diagnostic, DiagnosticLevel, Result};
use parsely_lexer::{
    tokens::{self, Token},
    AsSpan, Span,
};

use crate::{Brackets, Parens, Parse, ParseStream, Punctuation};

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
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        match stream.peek_expected("literal")? {
            Token::Int(_) => stream.parse().map(|s| Literal::Int(s)),
            Token::Float(_) => stream.parse().map(|s| Literal::Float(s)),
            Token::String(_) => stream.parse().map(|s| Literal::String(s)),
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: "literal".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralInt {
    pub value: tokens::Int,
}

impl Parse for LiteralInt {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        // let value = simple_attempt!(stream => [lit_int]);

        // Ok(LiteralInt { value })
        match stream.peek()? {
            Token::Int(i) => Ok(LiteralInt {
                value: stream.next_ref(i),
            }),
            tok => Err(Diagnostic::UnexpectedToken {
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
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        // let value = simple_attempt!(stream => [lit_float]);

        // Ok(LiteralFloat { value })
        match stream.peek()? {
            Token::Float(i) => Ok(LiteralFloat {
                value: stream.next_ref(i),
            }),
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::Float::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralString {
    pub value: tokens::String,
}

impl Parse for LiteralString {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        // let value = simple_attempt!(stream => [lit_string]);

        // Ok(LiteralString { value })

        match stream.peek()? {
            Token::String(i) => Ok(LiteralString {
                value: stream.next_ref(i),
            }),
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::String::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Poison,
    Literal(Literal),
    Ident(tokens::Ident),
    // ArrayInit(ArrayInit),
    Parens(Parens<Expression>),
    BinOp(BinOp),
}

impl AsSpan for Expression {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Expression::Poison => parsely_lexer::Span::EMPTY,
            Expression::Literal(l) => l.as_span(),
            Expression::BinOp(b) => b.as_span(),
            Expression::Ident(i) => i.as_span(),
            Expression::Parens(i) => i.value.as_span(),
            // Expression::ArrayInit(i) => i.as_span(),
        }
    }
}

impl Expression {
    fn parse_primary_expression(stream: &'_ mut ParseStream) -> Result<Expression> {
        match stream.peek_expected("parenthesis, identifier, or literal") {
            Ok(Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Paren,
                open,
                close,
                ..
            })) => {
                let span = open.join(*close);
                match stream.parse() {
                    Ok(parens) => Ok(Expression::Parens(parens)),
                    Err(_) => {
                        stream.push_diagnostic(Diagnostic::Message(
                            format!("Expected expression inside parenthesis"),
                            span,
                            DiagnosticLevel::Error,
                        ));

                        Ok(Expression::Parens(Parens {
                            parens: tokens::Paren { span },
                            value: Box::new(Expression::Poison),
                        }))
                    }
                }
            }
            // Ok(Token::Group(tokens::Group {
            //     bracket: tokens::GroupBracket::Bracket,
            //     ..
            // })) => stream.parse().map(|array| Expression::ArrayInit(array)),
            Ok(Token::Ident(ident)) => Ok(Expression::Ident(stream.next_ref(ident))),
            Ok(_) => {
                let res = stream.parse().map(Expression::Literal);

                match res {
                    Ok(expr) => Ok(expr),
                    Err(diag) => {
                        stream.push_diagnostic(diag);

                        Ok(Expression::Poison)
                    }
                }
            }
            Err(e) => {
                stream.push_diagnostic(e);

                Ok(Expression::Poison)
            }
        }
    }
}

impl Parse for Expression {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
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
        let left = self.left.as_span();
        let right = self.right.as_span();
        if left == Span::EMPTY {
            self.op.as_span().join(right)
        } else if right == Span::EMPTY {
            left.join(self.op.as_span())
        } else {
            left.join(right)
        }
    }
}

impl BinOp {
    fn parse_binop(stream: &'_ mut ParseStream, last_prec: usize) -> Result<Expression> {
        let mut left = Expression::parse_primary_expression(stream)?;

        while stream.has_next() && !stream.peek_eof() {
            let op = stream.peek().expect("stream should have token");

            let prec = BinOp::precedence(&op);

            if prec <= last_prec || prec == 0 {
                break;
            }

            let op = stream.next().expect("operator should exist");

            let right = BinOp::parse_binop(stream, prec).expect("This should never error");

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

// impl ArrayInit {
//     pub fn dimensions(&self) -> Vec<usize> {
//         let mut v = vec![self.elements.value.len()];

//         for f in self.elements.value.iter() {
//             match f {
//                 Expression::ArrayInit(a) => {
//                     v.extend(a.dimensions());
//                     break;
//                 }
//                 _ => (),
//             }
//         }

//         v
//     }
// }

// impl Parse for ArrayInit {
//     fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
//         Ok(ArrayInit {
//             elements: stream.parse()?,
//         })
//     }
// }

#[derive(Debug, Clone)]
pub struct Call {
    pub expr: Box<Expression>,
    pub args: Parens<Punctuation<Expression, tokens::Tok![,]>>,
}

#[cfg(test)]
mod test {
    use parsely_lexer::Lexer;

    use super::*;

    #[test]
    fn test_error() {
        // let tokens = Lexer::run("3 + (4 - + 5) - 6");
        // println!("{tokens:?}");
        // let mut ps = ParseStream::from(tokens);

        // let expr = Expression::parse(&mut ps).expect("Shouldn't error");

        // let diags = ps.finish();
        // println!("\nDiagnostics:");
        // for diag in diags {
        //     println!("{diag:?}");
        // }

        // println!("\nResult:");

        // println!("{expr:#?}");
    }
}
