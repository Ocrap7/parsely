use parsely_lexer::tokens::{self, Token};

use crate::{Parse, ParseError, Punctuation};

#[derive(Debug, Clone)]
pub enum Noun {
    Function(tokens::Function),
    Variable(tokens::Variable),
    Constant(tokens::Constant),
}

impl Parse for Noun {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Function(func) => Ok(Noun::Function(stream.next_ref(func))),
            Token::Variable(func) => Ok(Noun::Variable(stream.next_ref(func))),
            Token::Constant(func) => Ok(Noun::Constant(stream.next_ref(func))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "noun".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Indefinite {
    tok: tokens::A,
    noun: Noun,
}

impl Parse for Indefinite {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Indefinite {
            tok: stream.parse()?,
            noun: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Definite {
    tok: tokens::The,
    noun: Noun,
}

impl Parse for Definite {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Definite {
            tok: stream.parse()?,
            noun: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Value {
    the_tok: tokens::The,
    value_tok: tokens::Value,
    lit: Literal,
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

#[derive(Debug, Clone)]
pub struct ResultOf {
    the_tok: tokens::The,
    result: tokens::Result,
    of: tokens::Of,
    op: Operation,
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

#[derive(Debug, Clone)]
pub enum Expression {
    Value(Value),
    Result(ResultOf),
}

impl Parse for Expression {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peekn(1)? {
            tokens::Tok![enum value] => stream.parse().map(Expression::Value),
            tokens::Tok![enum result] => stream.parse().map(Expression::Result),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "expression".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    //     Add
}

impl Parse for Operation {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        todo!()
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Execute {
    exe_tok: tokens::Executes,
    what: Definite,
    called_tok: tokens::Called,
    ident: tokens::Ident,
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

#[derive(Debug, Clone)]
pub struct BlockList {
    statements: Punctuation<Statement, tokens::Then>,
}

impl Parse for BlockList {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(BlockList {
            statements: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct MutableInit {
    starts_tok: tokens::Starts,
    with_tok: tokens::With,
    value: Expression,
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

#[derive(Debug, Clone)]
pub struct ConstantInit {
    contains_tok: tokens::Contains,
    value: Expression,
}

impl Parse for ConstantInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ConstantInit {
            contains_tok: stream.parse()?,
            value: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Init {
    Function(BlockList),
    Variable(VariableInit),
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

#[derive(Debug, Clone)]
pub struct Definition {
    def_tok: tokens::Define,
    what: Indefinite,
    called_tok: tokens::Called,
    ident: tokens::Ident,
    that_tok: tokens::That,
    init: Init,
}

impl Parse for Definition {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Definition {
            def_tok: stream.parse()?,
            what: stream.parse()?,
            called_tok: stream.parse()?,
            ident: stream.parse()?,
            that_tok: stream.parse()?,
            init: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct DefinitionAction {
    def_tok: tokens::Defines,
    what: Indefinite,
    called_tok: tokens::Called,
    ident: tokens::Ident,
    that_tok: tokens::That,
    init: Init,
}

impl Parse for DefinitionAction {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(DefinitionAction {
            def_tok: stream.parse()?,
            what: stream.parse()?,
            called_tok: stream.parse()?,
            ident: stream.parse()?,
            that_tok: stream.parse()?,
            init: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    defs: Vec<Definition>,
}

impl Parse for Program {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Program {
            defs: stream.parse()?,
        })
    }
}
