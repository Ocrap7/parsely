use parsely_lexer::tokens::{self};

use crate::{statement::Statement, types::Type, Braces, Parens, Parse, Punctuation};

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Function(Function),
    ExternalFunction(ExternalFunction),
    Struct(Struct),
}

impl Parse for TopLevelItem {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum export] => match stream.peekn(1)? {
                tokens::Tok![enum opaque]
                | tokens::Tok![enum packed]
                | tokens::Tok![enum struct] => stream.parse().map(TopLevelItem::Struct),
                tokens::Tok![enum external] => stream.parse().map(TopLevelItem::ExternalFunction),
                _ => stream.parse().map(TopLevelItem::Function),
            },
            tokens::Tok![enum opaque] | tokens::Tok![enum packed] | tokens::Tok![enum struct] => {
                stream.parse().map(TopLevelItem::Struct)
            }
            tokens::Tok![enum external] => stream.parse().map(TopLevelItem::ExternalFunction),
            _ => stream.parse().map(TopLevelItem::Function),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub export: Option<tokens::Tok![export]>,
    pub opaque: Option<tokens::Tok![opaque]>,
    pub packed: Option<tokens::Tok![packed]>,
    pub keyword: tokens::Tok![struct],
    pub ident: tokens::Ident,
    pub body: Braces<Punctuation<Parameter, tokens::Tok![;]>>,
}

impl Parse for Struct {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Struct {
            export: stream.parse()?,
            opaque: stream.parse()?,
            packed: stream.parse()?,
            keyword: stream.parse()?,
            ident: stream.parse()?,
            body: Braces::parse_with(stream, Punctuation::parse_terminated)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub parameter_type: Box<Type>,
    pub ident: tokens::Ident,
}

impl Parse for Parameter {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Parameter {
            parameter_type: stream.parse()?,
            ident: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub export: Option<tokens::Tok![export]>,
    pub external: Option<tokens::Tok![external]>,
    pub return_type: Box<Type>,
    pub ident: tokens::Ident,
    pub params: Parens<Punctuation<Parameter, tokens::Tok![,]>>,
    pub body: Braces<Vec<Statement>>,
}

impl Parse for Function {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Function {
            export: stream.parse()?,
            external: stream.parse()?,
            return_type: stream.parse()?,
            ident: stream.parse()?,
            params: stream.parse()?,
            body: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ExternalFunction {
    pub export: Option<tokens::Tok![export]>,
    pub external: tokens::Tok![external],
    pub return_type: Box<Type>,
    pub ident: tokens::Ident,
    pub params: Parens<Punctuation<Parameter, tokens::Tok![,]>>,
    pub semi: tokens::Tok![;],
}

impl Parse for ExternalFunction {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ExternalFunction {
            export: stream.parse()?,
            external: stream.parse()?,
            return_type: stream.parse()?,
            ident: stream.parse()?,
            params: stream.parse()?,
            semi: stream.parse()?,
        })
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

#[cfg(test)]
mod test {
    use parsely_lexer::Lexer;

    use crate::ParseStream;

    use super::*;

    #[test]
    fn test_basic() {
        let input = r"
export struct data {
    int32 d;
}
";
        let tokens = Lexer::run(input.as_bytes());
        let stream = ParseStream::from(&tokens);

        let tli: TopLevelItem = stream.parse().unwrap();
        println!("{:#?}", tli);
    }
}
