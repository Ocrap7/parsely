use parsely_lexer::tokens::{self, Group, GroupBracket, Token};

use crate::{expression::Expression, types::Type, Braces, Brackets, Parse};

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(ExpressionStatement),
    VariableDeclaration(VariableDeclaration),
    IfStatement(IfStatement),
    WhileLoop(WhileLoop),
    ReturnStatement(ReturnStatement),
}

impl Parse for Statement {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match (stream.peek()?, stream.peekn(1)) {
            (Token::Ident(_), Ok(Token::Ident(_))) => {
                stream.parse().map(Statement::VariableDeclaration)
            }
            (tokens::Tok![enum if], _) => stream.parse().map(Statement::IfStatement),
            (tokens::Tok![enum while], _) => stream.parse().map(Statement::WhileLoop),
            (tokens::Tok![enum return], _) => stream.parse().map(Statement::ReturnStatement),
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

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub ty: Box<Type>,
    pub ident: tokens::Ident,
    pub arrays: Vec<ArrayDimension>,
    pub init: Option<VariableInit>,
    pub semi: tokens::Tok!(;),
}

impl Parse for VariableDeclaration {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let ty = stream.parse()?;
        let ident = stream.parse()?;

        let mut arrays = Vec::new();
        while let Token::Group(Group {
            bracket: GroupBracket::Bracket,
            ..
        }) = stream.peek()?
        {
            arrays.push(stream.parse()?);
        }

        let init = if let tokens::Tok![enum =] = stream.peek()? {
            Some(stream.parse()?)
        } else {
            None
        };

        Ok(VariableDeclaration {
            ty,
            ident,
            arrays,
            init,
            semi: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ArrayDimension {
    pub dimension: Brackets<Option<tokens::Int>>,
}

impl Parse for ArrayDimension {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ArrayDimension {
            dimension: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct VariableInit {
    pub equal: tokens::Tok![=],
    pub expression: Box<Expression>,
}

impl Parse for VariableInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(VariableInit {
            equal: stream.parse()?,
            expression: Box::new(stream.parse()?),
        })
    }
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub token: tokens::Tok![if],
    pub condition: Box<Expression>,
    pub body: Braces<Vec<Statement>>,
}

impl Parse for IfStatement {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(IfStatement {
            token: stream.parse()?,
            condition: stream.parse()?,
            body: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct WhileLoop {
    pub token: tokens::Tok![if],
    pub condition: Box<Expression>,
    pub body: Braces<Vec<Statement>>,
}

impl Parse for WhileLoop {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(WhileLoop {
            token: stream.parse()?,
            condition: stream.parse()?,
            body: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub token: tokens::Tok![return],
    pub expr: Box<Expression>,
    pub semi: tokens::Tok![;],
}

impl Parse for ReturnStatement {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ReturnStatement {
            token: stream.parse()?,
            expr: stream.parse()?,
            semi: stream.parse()?,
        })
    }
}
