use parsely_lexer::tokens;

use crate::{expr::Expression, Parse};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
}

impl Parse for Statement {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek() {
            _ => stream.parse().map(Statement::Expression),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
    pub semi: tokens::Tok!(;),
}

impl Parse for ExpressionStatement {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ExpressionStatement {
            expression: stream.parse()?,
            semi: stream.parse()?,
        })
    }
}
