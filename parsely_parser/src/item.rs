use parsely_lexer::tokens::{self};

use crate::{statement::Statement, types::Type, Braces, Parens, Parse, Punctuation};

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Function(Function),
    Struct(Struct),
}

impl Parse for TopLevelItem {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum export] => match stream.peekn(1)? {
                tokens::Tok![enum struct] => stream.parse().map(TopLevelItem::Struct),
                _ => stream.parse().map(TopLevelItem::Function),
            },
            tokens::Tok![enum struct] => stream.parse().map(TopLevelItem::Struct),
            _ => stream.parse().map(TopLevelItem::Function),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub export: Option<tokens::Tok![export]>,
    pub keyword: tokens::Tok![struct],
    pub ident: tokens::Ident,
    pub body: Braces<Punctuation<Parameter, tokens::Tok![;]>>,
}

impl Parse for Struct {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Struct {
            export: stream.parse()?,
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
    pub return_type: Box<Type>,
    pub ident: tokens::Ident,
    pub params: Parens<Punctuation<Parameter, tokens::Tok![,]>>,
    pub body: Braces<Statement>,
}

impl Parse for Function {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Function {
            export: stream.parse()?,
            return_type: stream.parse()?,
            ident: stream.parse()?,
            params: stream.parse()?,
            body: stream.parse()?,
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
