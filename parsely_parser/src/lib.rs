use std::{cell::Cell, fmt::Display};

use parsely_lexer::tokens::Token;

mod expr;
mod item;
mod statement;
mod tokens;
mod types;

/// Represents an error while parsing
#[derive(Debug)]
pub enum ParseError {
    /// Encountered an unexpected token while parsing
    UnexpectedToken {
        /// Found token 'found'
        found: Token,
        /// Expected this token. This is a string to support tokens with data values.
        expected: String,
    },
    /// No more input (EOF)
    UnexpectedEnd {
        // /// Expected this token. This is a string to support tokens with data values.
        // expected: String,
    },
    /// Encountered an unexpected size (such as in int32 size)
    UnexpectedSize { found: String },
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedSize { found } => {
                write!(f, "Expected size `{}`", found)
            }
            ParseError::UnexpectedEnd {} => {
                write!(f, "Unexpected end of input!")
            }
            ParseError::UnexpectedToken { found, expected } => {
                write!(f, "Expected token `{}`, found `{}`", found, expected)
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// Parsing result
pub type Result<T> = std::result::Result<T, ParseError>;

/// Trait that represents a parsable type.
/// This is implemented for all token types in `parsely_lexer::tokens`
pub trait Parse: Sized {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self>;
}

/// A stream of tokens that can be read from.
pub struct ParseStream<'a> {
    /// Token buffer
    buffer: &'a [Token],
    /// Current index into token buffer
    index: Cell<usize>,
}

impl ParseStream<'_> {
    /// Try and parse `T`
    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    pub fn parse_with<T: Parse>(&self, f: impl Fn(&'_ ParseStream<'_>) -> Result<T>) -> Result<T> {
        f(self)
    }

    pub fn has_next(&self) -> bool {
        self.index.get() < self.buffer.len()
    }

    /// Peek the current token
    pub fn peek(&self) -> Result<&Token> {
        self.buffer
            .get(self.index.get())
            .ok_or(ParseError::UnexpectedEnd {})
    }

    /// Peek the nth token from the current token
    pub fn peekn(&self, n: usize) -> Result<&Token> {
        self.buffer
            .get(self.index.get() + n)
            .ok_or(ParseError::UnexpectedEnd {})
    }

    /// Increment the current token without returning it
    pub fn increment(&self) {
        let value = self.index.take();
        self.index.set(value + 1);
    }

    /// Get the next token the cloned token
    pub fn next(&self) -> Token {
        let value = self.index.take();
        self.index.set(value + 1);
        self.buffer[value].clone()
    }

    /// Get the next token and return a direct token type
    pub fn next_ref<T: Clone>(&self, token: &T) -> T {
        let value = self.index.take();
        self.index.set(value + 1);
        token.clone()
    }
}

impl<'a> From<&'a [Token]> for ParseStream<'a> {
    fn from(value: &'a [Token]) -> Self {
        Self {
            buffer: value,
            index: Cell::new(0),
        }
    }
}

impl<'a> From<&'a Vec<Token>> for ParseStream<'a> {
    fn from(value: &'a Vec<Token>) -> Self {
        Self {
            buffer: value,
            index: Cell::new(0),
        }
    }
}

impl<T> Parse for Option<T>
where
    T: Parse,
{
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        Ok(T::parse(stream).ok())
    }
}

impl<T> Parse for Box<T>
where
    T: Parse,
{
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        Ok(Box::new(T::parse(stream)?))
    }
}

pub fn parse_from_vec(vec: &Vec<Token>) -> ParseStream<'_> {
    ParseStream {
        buffer: &vec,
        index: Cell::new(0),
    }
}

#[derive(Debug, Clone)]
pub struct Parens<T: Parse> {
    pub parens: parsely_lexer::tokens::Paren,
    pub value: Box<T>,
}

impl<T: Parse> Parse for Parens<T> {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        match stream.next() {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Paren,
                tokens,
                open,
                close,
                ..
            }) => {
                let stream = crate::parse_from_vec(&tokens);
                stream.parse().map(|value| Parens {
                    parens: parsely_lexer::tokens::Paren {
                        span: open.join(close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(ParseError::UnexpectedToken {
                found,
                expected: "Parenthesis".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Braces<T: Parse> {
    pub parens: parsely_lexer::tokens::Brace,
    pub value: Box<T>,
}

impl<T: Parse> Braces<T> {
    fn parse_with(
        stream: &'_ ParseStream<'_>,
        f: impl Fn(&'_ ParseStream<'_>) -> Result<T>,
    ) -> Result<Self> {
        match stream.next() {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Brace,
                tokens,
                open,
                close,
                ..
            }) => {
                let stream = crate::parse_from_vec(&tokens);
                f(&stream).map(|value| Braces {
                    parens: parsely_lexer::tokens::Brace {
                        span: open.join(close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(ParseError::UnexpectedToken {
                found,
                expected: "Braces".to_string(),
            }),
        }
    }
}

impl<T: Parse> Parse for Braces<T> {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        match stream.next() {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Brace,
                tokens,
                open,
                close,
                ..
            }) => {
                let stream = crate::parse_from_vec(&tokens);
                stream.parse().map(|value| Braces {
                    parens: parsely_lexer::tokens::Brace {
                        span: open.join(close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(ParseError::UnexpectedToken {
                found,
                expected: "Braces".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Brackets<T: Parse> {
    pub parens: parsely_lexer::tokens::Bracket,
    pub value: Box<T>,
}

impl<T: Parse> Parse for Brackets<T> {
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        match stream.next() {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Bracket,
                tokens,
                open,
                close,
                ..
            }) => {
                let stream = crate::parse_from_vec(&tokens);
                stream.parse().map(|value| Brackets {
                    parens: parsely_lexer::tokens::Bracket {
                        span: open.join(close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(ParseError::UnexpectedToken {
                found,
                expected: "Brackets".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Punctuation<T: Parse, P: Parse> {
    items: Vec<(T, P)>,
    last: Option<Box<T>>,
}

impl<T, P> Punctuation<T, P>
where
    T: Parse,
    P: Parse,
{
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items
            .iter()
            .map(|item| &item.0)
            .chain(self.last.as_ref().map(|last| last.as_ref()))
    }

    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.items
            .into_iter()
            .map(|item| item.0)
            .chain(self.last.map(|last| *last))
    }

    pub fn iter_punct(&self) -> impl Iterator<Item = &P> {
        self.items.iter().map(|item| &item.1)
    }

    pub fn into_iter_punct(self) -> impl Iterator<Item = P> {
        self.items.into_iter().map(|item| item.1)
    }

    pub fn iter_both(&self) -> impl Iterator<Item = (&T, Option<&P>)> {
        self.items
            .iter()
            .map(|item| (&item.0, Some(&item.1)))
            .chain(self.last.as_ref().map(|last| (last.as_ref(), None)))
    }

    pub fn into_iter_both(self) -> impl Iterator<Item = (T, Option<P>)> {
        self.items
            .into_iter()
            .map(|item| (item.0, Some(item.1)))
            .chain(self.last.map(|last| (*last, None)))
    }

    pub fn parse_terminated(stream: &'_ ParseStream<'_>) -> Result<Self> {
        let mut items = Vec::new();

        while stream.has_next() {
            let item = stream.parse()?;
            let punct = stream.parse()?;
            items.push((item, punct));
        }

        Ok(Punctuation { items, last: None })
    }
}

impl<T, P> Parse for Punctuation<T, P>
where
    T: Parse,
    P: Parse,
{
    fn parse(stream: &'_ ParseStream<'_>) -> Result<Self> {
        let mut items = Vec::new();
        let mut last = None;

        while stream.has_next() {
            let item = stream.parse()?;
            if stream.has_next() {
                let punct = stream.parse()?;
                items.push((item, punct));
            } else {
                last = Some(Box::new(item));
            }
        }

        Ok(Punctuation { items, last })
    }
}

#[cfg(test)]
mod test {
    fn test_basic() {}
}
