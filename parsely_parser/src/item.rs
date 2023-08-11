use parsely_lexer::tokens::{self, GroupBracket, Token};

use crate::{expression::Expression, Braces, Parens, Parse, Punctuation};

#[derive(Debug, Clone)]
pub struct Argument {
    pub key: tokens::Ident,
    pub value: Option<(tokens::Tok![:], Box<Expression>)>,
}

impl Parse for Argument {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let key = stream.parse()?;

        let value = if let Ok(tokens::Tok![enum :]) = stream.peek() {
            Some((stream.parse()?, stream.parse()?))
        } else {
            None
        };

        Ok(Argument { key, value })
    }
}

#[derive(Debug, Clone)]
pub struct Arguments(Parens<Punctuation<Argument, tokens::Tok![,]>>);

impl Parse for Arguments {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        stream.parse().map(Arguments)
    }
}

#[derive(Debug, Clone)]
pub struct ElementChild {
    pub child: Option<Box<Expression>>,
    pub semi: tokens::Tok![;],
}

impl Parse for ElementChild {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        if let Ok(tokens::Tok![enum ; as semi]) = stream.peek() {
            Ok(ElementChild {
                child: None,
                semi: stream.next_ref(semi),
            })
        } else {
            Ok(ElementChild {
                child: stream.parse()?,
                semi: stream.parse()?,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub enum ElementBody {
    Children(Braces<Item>),
    Child(ElementChild),
}

impl Parse for ElementBody {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        if let Ok(Token::Group(tokens::Group {
            bracket: GroupBracket::Brace,
            ..
        })) = stream.peek()
        {
            stream.parse().map(ElementBody::Children)
        } else {
            stream.parse().map(ElementBody::Child)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Element {
    pub tag: tokens::Ident,
    pub args: Option<Arguments>,
    pub body: ElementBody,
}

impl Parse for Element {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Element {
            tag: stream.parse()?,
            args: stream.parse()?,
            body: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Element(Element),
    Expression(Box<Expression>),
}

impl Parse for Item {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Ident(_) => {
                stream.parse().map(Item::Element)
            }
            _ => stream.parse().map(Item::Element),
        }
    }
}

#[cfg(test)]
mod test {}
