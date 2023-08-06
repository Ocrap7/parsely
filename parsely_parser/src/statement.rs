use parsely_lexer::tokens;
use parsely_macros::AsSpan;

use crate::{
    expression::Expression,
    item::{Arguments, Definite, Indefinite},
    Parse, ParseError, Punctuation,
};

#[derive(Debug, Clone, AsSpan)]
pub enum Statement {
    Definition(DefinitionAction),
    Execute(Execute),
}

impl Parse for Statement {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum defines] => Ok(Statement::Definition(stream.parse()?)),
            tokens::Tok![enum executes] => Ok(Statement::Execute(stream.parse()?)),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "statement".into(),
            }),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct Execute {
    pub exe_tok: tokens::Executes,
    pub what: Definite,
    pub called_tok: tokens::Called,
    pub ident: tokens::Ident,
}

impl Parse for Execute {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Execute {
            exe_tok: stream.parse()?,
            what: stream.parse()?,
            called_tok: stream.parse()?,
            ident: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct BlockList {
    pub statements: Punctuation<Statement, tokens::Then>,
}

impl Parse for BlockList {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(BlockList {
            statements: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct MutableInit {
    pub starts_tok: tokens::Starts,
    pub with_tok: tokens::With,
    pub value: Expression,
}

impl Parse for MutableInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(MutableInit {
            starts_tok: stream.parse()?,
            with_tok: stream.parse()?,
            value: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct ConstantInit {
    pub contains_tok: tokens::Contains,
    pub value: Expression,
}

impl Parse for ConstantInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ConstantInit {
            contains_tok: stream.parse()?,
            value: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum VariableInit {
    Mutable(MutableInit),
    Const(ConstantInit),
}

impl Parse for VariableInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum starts] => Ok(VariableInit::Mutable(stream.parse()?)),
            tokens::Tok![enum contains] => Ok(VariableInit::Const(stream.parse()?)),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "variable initializer".into(),
            }),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub enum Init {
    Variable(VariableInit),
    Function(BlockList),
}

impl Init {
    pub fn as_var(&self) -> &VariableInit {
        match self {
            Init::Variable(var) => var,
            _ => panic!("Expected variable!"),
        }
    }

    pub fn as_fn(&self) -> &BlockList {
        match self {
            Init::Function(func) => func,
            _ => panic!("Expected fucntion!"),
        }
    }
}

impl Parse for Init {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum starts] | tokens::Tok![enum contains] => {
                Ok(Init::Variable(stream.parse()?))
            }
            _ => Ok(Init::Function(stream.parse()?)),
        }
    }
}

#[derive(Debug, Clone, AsSpan)]
pub struct DefinitionAction {
    pub def_tok: tokens::Defines,
    pub what: Indefinite,
    pub called_tok: tokens::Called,
    pub ident: tokens::Ident,
    pub args: Option<Arguments>,
    pub that_tok: tokens::That,
    pub init: Init,
}

impl Parse for DefinitionAction {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let def_tok = stream.parse()?;
        let what: Indefinite = stream.parse()?;

        Ok(DefinitionAction {
            called_tok: stream.parse()?,
            ident: stream.parse()?,
            args: match what.noun.is_fn().then(|| stream.parse()) {
                Some(s) => s?,
                None => None,
            },
            that_tok: stream.parse()?,
            init: stream.parse()?,
            def_tok,
            what,
        })
    }
}
