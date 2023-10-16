use parsely_lexer::{
    tokens::{self, Comma, GroupBracket, Token},
    AsSpan,
};

use crate::{expression::Expression, Braces, Parens, Parse, Punctuation};

#[derive(Debug, Clone)]
pub struct Argument {
    pub key: tokens::Ident,
    pub value: Option<(tokens::Tok![:], Box<Expression>)>,
}

impl AsSpan for Argument {
    fn as_span(&self) -> parsely_lexer::Span {
        self.key.as_span().join(self.value.as_span())
    }
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
pub struct Arguments(pub Parens<Punctuation<Argument, tokens::Tok![,]>>);

impl Parse for Arguments {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        stream.parse().map(Arguments)
    }
}

impl AsSpan for Arguments {
    fn as_span(&self) -> parsely_lexer::Span {
        self.0.parens.span
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

impl AsSpan for ElementChild {
    fn as_span(&self) -> parsely_lexer::Span {
        self.child.as_span()
    }
}

#[derive(Debug, Clone)]
pub enum ElementBody {
    Children(Braces<Vec<Item>>),
    Child(ElementChild),
}

impl AsSpan for ElementBody {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            ElementBody::Child(child) => child.as_span(),
            ElementBody::Children(children) => children.parens.span,
        }
    }
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

impl AsSpan for Element {
    fn as_span(&self) -> parsely_lexer::Span {
        self.tag.as_span().join(self.body.as_span())
    }
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
pub struct Input {
    pub ident: tokens::Ident,
    pub opt: Option<tokens::Tok![?]>,
    pub colon: tokens::Tok![:],
    pub template: tokens::Template,
}

impl AsSpan for Input {
    fn as_span(&self) -> parsely_lexer::Span {
        self.ident.as_span().join(self.template.as_span())
    }
}

impl Parse for Input {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Input {
            ident: stream.parse()?,
            opt: stream.parse()?,
            colon: stream.parse()?,
            template: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Inputs {
    pub input_tok: tokens::Parameters,
    pub inputs: Braces<Punctuation<Input, Comma>>,
}

impl AsSpan for Inputs {
    fn as_span(&self) -> parsely_lexer::Span {
        self.input_tok.as_span().join(self.inputs.parens.span)
    }
}

impl Parse for Inputs {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Inputs {
            input_tok: stream.parse()?,
            inputs: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Element(Element),
    Expression(Box<Expression>),
    Inputs(Inputs),
}

impl AsSpan for Item {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Item::Element(elem) => elem.as_span(),
            Item::Expression(expr) => expr.as_span(),
            Item::Inputs(inputs) => inputs.as_span(),
        }
    }
}

impl Parse for Item {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Ident(_) => stream.parse().map(Item::Element),
            tokens::Tok![enum parameters] => stream.parse().map(Item::Inputs),
            _ => stream.parse().map(Item::Expression),
        }
    }
}

#[cfg(test)]
mod test {}
