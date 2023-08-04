use parsely_lexer::tokens::{self, Token};

use crate::{statement::Init, types, Parse, ParseError, Punctuation};

#[derive(Debug, Clone)]
pub enum Noun {
    Function(tokens::Function),
    Variable(tokens::Variable),
    Constant(tokens::Constant),
}

impl Noun {
    pub fn is_fn(&self) -> bool {
        matches!(self, Noun::Function(_))
    }
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
    pub tok: tokens::A,
    pub noun: Noun,
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
    pub tok: tokens::The,
    pub noun: Noun,
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
pub struct Argument {
    pub ident: tokens::Ident,
    pub ty: types::OfType,
}

impl Parse for Argument {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Argument {
            ident: stream.parse()?,
            ty: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Arguments {
    pub with_tok: tokens::With,
    pub args_tok: tokens::Arguments,
    pub args: Punctuation<Argument, tokens::Comma>,
}

impl Parse for Arguments {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Arguments {
            with_tok: stream.parse()?,
            args_tok: stream.parse()?,
            args: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub def_tok: tokens::Define,
    pub what: Indefinite,
    pub called_tok: tokens::Called,
    pub ident: tokens::Ident,
    pub args: Option<Arguments>,
    pub that_tok: tokens::That,
    pub init: Init,
}

impl Parse for Definition {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let def_tok = stream.parse()?;
        let what: Indefinite = stream.parse()?;
        let called_tok = stream.parse()?;
        let ident = stream.parse()?;

        let args = if what.noun.is_fn() && matches!(stream.peek()?, Token::With(_)) {
            Some(stream.parse()?)
        } else {
            None
        };

        Ok(Definition {
            def_tok,
            what,
            called_tok,
            ident,
            args,
            that_tok: stream.parse()?,
            init: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Definition(Definition),
}

impl Parse for TopLevelItem {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum define] => stream.parse().map(TopLevelItem::Definition),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "top level item".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<TopLevelItem>,
}

impl Parse for Program {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Program {
            items: stream.parse()?,
        })
    }
}
