use parsely_lexer::tokens::{self};

use crate::{expression::Expression, Brackets, Parse, Punctuation};

#[derive(Debug, Clone)]
pub struct Arguments {
    pub values: Punctuation<tokens::Ident, tokens::Token>,
}

impl Parse for Arguments {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Arguments {
            values: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Op {
    pub op: tokens::Ident,
    pub args: Expression,
}

impl Parse for Op {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Op {
            op: stream.parse()?,
            args: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct OpList {
    pub list: Punctuation<Op, tokens::Comma>,
}

impl Parse for OpList {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(OpList {
            list: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum BoundValue {
    Expression(Expression),
    OpList(OpList),
}

impl Parse for BoundValue {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let expr = stream.try_parse_with::<Expression>(|stream| {
            let expr = stream.parse();
            if let (Ok(expr), Ok(tokens::Tok![enum ;])) = (expr, stream.peek()) {
                Ok(expr)
            } else {
                Err(crate::ParseError::UnexpectedEnd {})
            }
        });

        if let Ok(expr) = expr {
            Ok(BoundValue::Expression(expr))
        } else {
            stream.parse().map(BoundValue::OpList)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub let_tok: tokens::Tok![let],
    pub ident: tokens::Ident,
    pub eq_tok: tokens::Tok![=],
    pub value: BoundValue,
    pub semi_tok: tokens::Tok![;],
}

impl Parse for Binding {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Binding {
            let_tok: stream.parse()?,
            ident: stream.parse()?,
            eq_tok: {
                let val = stream.parse()?;
                stream.ignore_nl();
                val
            },
            value: stream.parse()?,
            semi_tok: {
                stream.ignore_nl();
                stream.parse()?
            },
        })
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub shebang_tok: tokens::Shebang,
    pub value: Brackets<Expression>,
}

impl Parse for Attribute {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Attribute {
            shebang_tok: stream.parse()?,
            value: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Attribute(Attribute),
    Binding(Binding),
}

impl Parse for Item {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        stream.ignore_nl();
        match stream.peek()? {
            tokens::Tok![enum let] => stream.parse().map(Item::Binding),
            tok => Err(crate::ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "item".into(),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use parsely_lexer::{
        span,
        tokens::{Bracket, Ident, Token},
        Lexer,
    };

    use crate::ParseStream;

    use super::*;

    fn lx(st: &str) -> Vec<Token> {
        Lexer::run(st)
    }

    fn prs<T: Parse>(st: &str) -> T {
        let toks = lx(st);
        println!("toks: {toks:#?}");
        let ps = ParseStream::from(&toks);

        T::parse(&ps).unwrap()
    }

    #[test]
    fn test_attribute() {
        let attr = prs::<Attribute>("#![tok]");

        println!("{:#?}", attr);
        // assert_eq!(
        //     attr,
        //     Attribute {
        //         shebang_tok: tokens::Shebang(span!(0:0-2)),
        //         value: Brackets {
        //             parens: Bracket { span: span!(0:2-7) },
        //             value: Expression::Ident(Ident {
        //                 value: "tok".into(),
        //                 span: span!(0:3-6),
        //             })
        //         }
        //     }
        // );
    }

    #[test]
    fn test_binding() {
        let binding = prs::<Binding>("let c = add x0 = x0 + x1;");

        println!("{:#?}", binding);
    }
}
