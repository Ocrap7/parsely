use std::{
    cell::{Cell, Ref, RefCell},
    fmt::Debug,
    rc::Rc,
};

use dummy::Dummy;
use parsely_diagnostics::{Diagnostic, DiagnosticLevel, Result};
use parsely_lexer::{
    tokens::{GroupBracket, Token},
    AsSpan, Position, Span,
};

pub mod dummy;
pub mod expression;
pub mod item;
pub mod tokens;

pub mod program;

// pub type Result<T> = std::result::Result<T, T>;

// pub enum Result<T> {
//     Success(T),
//     Dummy(T),
//     Diagnostic(Diagnostic),
// }

// impl<T> Result<T> {
//     fn result(self) -> std::result::Result<T, Diagnostic> {
//         match self {
//             Result::Success(t) => Ok(t),
//             Result::Dummy(t) => Ok(t),
//             Result::Diagnostic(t) => Err(t),
//         }
//     }

//     fn value(self) -> Option<T> {
//         match self {
//             Result::Success(t) => Some(t),
//             Result::Dummy(t) => Some(t),
//             Result::Diagnostic(_) => None,
//         }
//     }
// }

// pub trait ResultExt<T> {
//     fn zip(t: T) -> Self;
//     fn map_one<U>(self, f: impl Fn(T) -> U) -> Result<U>;
//     fn unzip(self) -> T;
// }

// impl<T> ResultExt<T> for Result<T> {
//     fn zip(t: T) -> Self {
//         Ok(t)
//     }

//     fn map_one<U>(self, f: impl Fn(T) -> U) -> Result<U> {
//         self.map(f).map_err(f)
//     }

//     fn unzip(self) -> T {
//         match self {
//             Ok(t) => t,
//             Err(t) => t,
//         }
//     }
// }

/// Trait that represents a parsable type.
/// This is implemented for all token types in `parsely_lexer::tokens`
pub trait Parse: Sized {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self>;
}

const MAX_ATTEMPTS: u8 = 5;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RestorePoint(usize);

/// A stream of tokens that can be read from.
pub struct ParseStream {
    /// Token buffer
    buffer: Vec<Token>,
    /// Current index into token buffer
    index: Cell<usize>,
    /// Accumulated diagnostics
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
    ///
    until_terminator: Option<Token>,
    ///
    sub_span: Option<Span>,
}

impl ParseStream {
    /// Try and parse `T`
    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

    pub fn parse_with<T>(&mut self, f: impl Fn(&'_ mut ParseStream) -> Result<T>) -> Result<T> {
        f(self)
    }

    /// Trys to parse `T` but if it fails, the token index and diagnostic index will be restored in case parsing T consumed tokens
    pub fn try_parse<T: Parse>(&mut self) -> Result<T> {
        let index = self.index.get();
        let diagnostic_len = self.diagnostics.borrow().len();

        match T::parse(self) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.index.set(index);
                self.diagnostics.borrow_mut().truncate(diagnostic_len);
                Err(e)
            }
        }
    }

    /// Trys to parse `T` but if it fails, the token index and diagnostic index will be restored in case parsing T consumed tokens
    pub fn try_parse_with<T: Parse>(
        &mut self,
        f: impl Fn(&'_ mut ParseStream) -> Result<T>,
    ) -> Result<T> {
        let index = self.index.get();
        let diagnostic_len = self.diagnostics.borrow().len();

        match self.parse_with(f) {
            Ok(t) => Ok(t),
            Err(e) => {
                self.index.set(index);
                self.diagnostics.borrow_mut().truncate(diagnostic_len);
                Err(e)
            }
        }
    }

    #[must_use]
    pub fn insert(&mut self, token: impl Into<Token>) -> RestorePoint {
        let index = self.index.get();
        self.buffer.insert(index, token.into());

        RestorePoint(index)
    }

    pub fn insert_at(&mut self, token: impl Into<Token>, restore_point: RestorePoint) {
        self.buffer.insert(restore_point.0, token.into());
    }

    #[must_use]
    pub fn current_point(&self) -> RestorePoint {
        RestorePoint(self.index.get())
    }

    pub fn restore(&mut self, restore_point: RestorePoint) -> RestorePoint {
        RestorePoint(self.index.replace(restore_point.0))
    }

    pub fn remove(&mut self, restore_point: RestorePoint) -> Token {
        self.buffer.remove(restore_point.0)
    }

    #[must_use]
    pub fn remove_current(&mut self) -> (Token, RestorePoint) {
        (
            self.buffer.remove(self.index.get()),
            RestorePoint(self.index.get()),
        )
    }

    pub fn has_next(&self) -> bool {
        self.index.get() < self.buffer.len()
    }

    /// Peek the current token
    pub fn peek(&self) -> parsely_diagnostics::Result<&Token> {
        let token = self.buffer.get(self.index.get());
        match token {
            Some(Token::Eof(span)) => Err(Diagnostic::UnexpectedEnd(*span)),
            Some(tok) => Ok(tok),
            None => Err(Diagnostic::UnexpectedEnd(
                self.buffer
                    .last()
                    .map(|tok| tok.as_span())
                    .unwrap_or(Span::EMPTY),
            )),
        }
    }

    pub fn peek_expected(&self, expected: &str) -> parsely_diagnostics::Result<&Token> {
        let token = self.buffer.get(self.index.get());
        match token {
            Some(tok @ Token::Eof(_)) => {
                if let Some(span) = self.sub_span {
                    Err(Diagnostic::Message(
                        format!("Unexpected end of input for group"),
                        span,
                        DiagnosticLevel::Error,
                    ))
                } else {
                    Err(Diagnostic::UnexpectedToken {
                        found: tok.clone(),
                        expected: expected.to_string(),
                    })
                }
            }
            Some(tok) => Ok(tok),
            None => {
                if let Some(span) = &self.sub_span {
                    println!("{:?}", span);
                    Err(Diagnostic::Message(
                        format!("Unexpected end of input for group"),
                        *span,
                        DiagnosticLevel::Error,
                    ))
                } else {
                    Err(Diagnostic::UnexpectedToken {
                        found: self
                            .buffer
                            .last()
                            .cloned()
                            .unwrap_or_else(|| Token::Eof(Span::EMPTY)),
                        expected: expected.to_string(),
                    })
                }
            }
        }
    }

    /// Peek the nth token from the current token
    pub fn peekn(&self, n: usize) -> parsely_diagnostics::Result<&Token> {
        let token = self.buffer.get(self.index.get() + n);
        match token {
            Some(Token::Eof(span)) => Err(Diagnostic::UnexpectedEnd(*span)),
            Some(tok) => Ok(tok),
            None => Err(Diagnostic::UnexpectedEnd(
                self.buffer
                    .last()
                    .map(|tok| tok.as_span())
                    .unwrap_or(Span::EMPTY),
            )),
        }
    }

    pub fn push_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.borrow_mut().push(diagnostic);
    }

    pub fn last(&self) -> &Token {
        let tok = self.buffer.last().unwrap();
        assert!(matches!(tok, Token::Eof(_)));

        tok
    }

    /// Returns true if peeked token is eof
    pub fn peek_eof(&self) -> bool {
        matches!(self.buffer.get(self.index.get()), Some(Token::Eof(_)))
    }

    pub fn ignore_nl(&self) {
        while let Ok(Token::Newline(_)) = self.peek() {
            self.increment();
        }
    }

    /// Increment the current token without returning it
    pub fn increment(&self) {
        let value = self.index.take();
        self.index.set(value + 1);
    }

    /// Get the next token the cloned token
    pub fn next(&self) -> Result<Token> {
        let value = self.index.take();
        self.index.set(value + 1);

        self.buffer
            .get(value)
            .cloned()
            .ok_or_else(|| Diagnostic::UnexpectedToken {
                found: self.buffer.last().cloned().unwrap(),
                expected: "token".to_string(),
            })
    }

    /// Get the next token and return a direct token type
    pub fn next_ref<T: Clone>(&self, token: &T) -> T {
        let value = self.index.take();
        self.index.set(value + 1);
        token.clone()
    }

    pub fn current_position(&self) -> Position {
        let buffer = &self.buffer;

        buffer
            .get(self.index.get())
            .or_else(|| buffer.get(self.index.get() - 1))
            .map(|tok| tok.as_span().start)
            .unwrap()
    }

    pub fn fork(&self, span: Span, tokens: &[Token]) -> ParseStream {
        ParseStream {
            buffer: tokens.to_vec(),
            index: Cell::new(0),
            diagnostics: self.diagnostics.clone(),
            until_terminator: None,
            sub_span: Some(span),
        }
    }

    pub fn finish(self) -> Vec<Diagnostic> {
        self.diagnostics.take()
    }
}

impl From<Vec<Token>> for ParseStream {
    fn from(value: Vec<Token>) -> Self {
        Self {
            buffer: value,
            index: Cell::new(0),
            diagnostics: Rc::new(RefCell::new(Vec::new())),
            until_terminator: None,
            sub_span: None,
        }
    }
}

impl<'a> From<&'a [Token]> for ParseStream {
    fn from(value: &'a [Token]) -> Self {
        Self {
            buffer: value.to_vec(),
            index: Cell::new(0),
            diagnostics: Rc::new(RefCell::new(Vec::new())),
            until_terminator: None,
            sub_span: None,
        }
    }
}

impl<'a> From<&'a Vec<Token>> for ParseStream {
    fn from(value: &'a Vec<Token>) -> Self {
        Self {
            buffer: value.to_vec(),
            index: Cell::new(0),
            diagnostics: Rc::new(RefCell::new(Vec::new())),
            until_terminator: None,
            sub_span: None,
        }
    }
}

// impl<T> Parse for Option<T>
// where
//     T: Parse,
// {
//     fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
//         stream.try_parse()
//     }
// }

impl<T> Parse for Box<T>
where
    T: Parse,
{
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        stream.parse().map(Box::new)
    }
}

impl<T> Parse for Vec<T>
where
    T: Parse + Debug,
{
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        let mut items = Vec::new();
        while stream.has_next() && !stream.peek_eof() {
            let item = match stream.parse::<T>() {
                Ok(v) => v,
                Err(d) => {
                    stream.push_diagnostic(d);

                    stream.increment();
                    continue;
                },
            };

            items.push(item);
        }

        Ok(items)
    }
}

#[derive(Debug, Clone)]
pub struct Parens<T: Parse> {
    pub parens: parsely_lexer::tokens::Paren,
    pub value: Box<T>,
}

impl<T: Parse> Parse for Parens<T> {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        match stream.peek()? {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Paren,
                tokens,
                open,
                close,
                ..
            }) => {
                stream.increment();

                let mut stream = stream.fork(open.join(*close), tokens);
                stream.parse().map(|value| Parens {
                    parens: parsely_lexer::tokens::Paren {
                        span: open.join(*close),
                    },
                    value: Box::new(value),
                })
            }
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: "parenthesis".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Braces<T: Parse> {
    pub braces: parsely_lexer::tokens::Brace,
    pub value: Box<T>,
}

impl<T: Parse> Braces<T> {
    fn parse_with(
        stream: &'_ mut ParseStream,
        f: impl Fn(&'_ mut ParseStream) -> Result<T>,
    ) -> Result<Self> {
        match stream.peek()? {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Brace,
                tokens,
                open,
                close,
                ..
            }) => {
                stream.increment();

                let mut stream = stream.fork(open.join(*close), tokens);
                f(&mut stream).map(|value| Braces {
                    braces: parsely_lexer::tokens::Brace {
                        span: open.join(*close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(Diagnostic::UnexpectedToken {
                found: found.clone(),
                expected: "Braces".to_string(),
            }),
        }
    }
}

impl<T: Parse> Parse for Braces<T> {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        match stream.peek()? {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Brace,
                tokens,
                open,
                close,
                ..
            }) => {
                stream.increment();

                let mut stream = stream.fork(open.join(*close), tokens);
                stream.parse().map(|value| Braces {
                    braces: parsely_lexer::tokens::Brace {
                        span: open.join(*close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(Diagnostic::UnexpectedToken {
                found: found.clone(),
                expected: "Braces".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Brackets<T> {
    pub brackets: parsely_lexer::tokens::Bracket,
    pub value: Box<T>,
}

impl<T: Parse> Parse for Brackets<T> {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        match stream.peek()? {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Bracket,
                tokens,
                open,
                close,
                ..
            }) => {
                stream.increment();

                let mut stream = stream.fork(open.join(*close), tokens);
                stream.parse().map(|value| Brackets {
                    brackets: parsely_lexer::tokens::Bracket {
                        span: open.join(*close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(Diagnostic::UnexpectedToken {
                found: found.clone(),
                expected: "Brackets".to_string(),
            }),
        }
    }
}

impl<T> Brackets<T> {
    fn parse_with(
        stream: &'_ mut ParseStream,
        f: impl Fn(&'_ mut ParseStream) -> Result<T>,
    ) -> Result<Self> {
        match stream.peek()? {
            Token::Group(parsely_lexer::tokens::Group {
                bracket: parsely_lexer::tokens::GroupBracket::Bracket,
                tokens,
                open,
                close,
                ..
            }) => {
                stream.increment();

                let mut stream = stream.fork(open.join(*close), tokens);
                f(&mut stream).map(|value| Brackets {
                    brackets: parsely_lexer::tokens::Bracket {
                        span: open.join(*close),
                    },
                    value: Box::new(value),
                })
            }
            found => Err(Diagnostic::UnexpectedToken {
                found: found.clone(),
                expected: "Brackets".to_string(),
            }),
        }
    }
}

impl<T> AsSpan for Brackets<T> {
    fn as_span(&self) -> Span {
        self.brackets.span
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
    P: Parse + Dummy + PartialEq<Token>,
{
    pub fn len(&self) -> usize {
        self.items.len() + if self.last.is_some() { 1 } else { 0 }
    }

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

    pub fn parse_terminated(stream: &'_ mut ParseStream) -> Result<Self> {
        let mut items = Vec::new();

        while stream.has_next() {
            if let Ok(item) = stream.parse() {
                let punct = stream.parse()?;
                items.push((item, punct));
            } else {
                let dummy = P::new_dummy(Position::EMPTY);

                while stream.has_next() {
                    if let Ok(token) = stream.next() {
                        if dummy == token {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(Punctuation { items, last: None })
    }
}

impl<T, P> Punctuation<T, P>
where
    T: Parse,
    P: Parse + Dummy + PartialEq<Token> + AsRef<str>,
{
    fn parse_end_terminator<E>(stream: &'_ mut ParseStream) -> Result<Self>
    where
        E: Parse + Dummy + PartialEq<Token> + AsRef<str>,
    {
        let mut items = Vec::new();
        let mut last = None;

        while stream.has_next() {
            if let Ok(item) = stream.try_parse() {
                let punct = stream.parse();
                match punct {
                    Ok(punct) => items.push((item, punct)),
                    _ => {
                        let dummy = E::new_dummy(Position::EMPTY);

                        if let Ok(tok) = stream.peek() {
                            if &dummy == tok {
                                last = Some(Box::new(item));
                                break;
                            }
                        }

                        let dummy_punct = P::new_dummy(stream.current_position());

                        stream.push_diagnostic(Diagnostic::UnexpectedToken {
                            found: stream.peek()?.clone(),
                            expected: format!("{} or {}", dummy_punct.as_ref(), dummy.as_ref()),
                        });

                        items.push((item, dummy_punct))
                    }
                }
            } else if last.is_none() {
                break;
            } else {
                let dummy = P::new_dummy(Position::EMPTY);

                while stream.has_next() {
                    if let Ok(token) = stream.next() {
                        if dummy == token {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(Punctuation { items, last })
    }
}

impl<T, P> Parse for Punctuation<T, P>
where
    T: Parse,
    P: Parse + Dummy + PartialEq<Token> + AsRef<str>,
{
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        let mut items = Vec::new();
        let mut last = None;

        while stream.has_next() {
            if let Ok(item) = stream.try_parse() {
                let punct = stream.parse();
                match punct {
                    Ok(punct) => items.push((item, punct)),
                    _ => {
                        last = Some(Box::new(item));
                        break;
                    }
                }
            } else if last.is_none() {
                break;
            } else {
                let dummy = P::new_dummy(Position::EMPTY);
                let found = stream.peek()?.clone();

                while stream.has_next() {
                    if let Ok(token) = stream.next() {
                        if dummy == token {
                            stream.push_diagnostic(Diagnostic::UnexpectedToken {
                                found,
                                expected: dummy.as_ref().to_string(),
                            });

                            break;
                        }
                    } else {
                        stream.push_diagnostic(Diagnostic::UnexpectedEnd(found.as_span()));
                        break;
                    }
                }
            }
        }

        Ok(Punctuation { items, last })
    }
}

impl<T, P> parsely_lexer::AsSpan for Punctuation<T, P>
where
    T: Parse + parsely_lexer::AsSpan,
    P: Parse + parsely_lexer::AsSpan,
{
    fn as_span(&self) -> parsely_lexer::Span {
        match &self.last {
            Some(l) if self.items.is_empty() => l.as_span(),
            Some(l) => self.items.as_span().join(l.as_span()),
            None if self.items.is_empty() => Default::default(),
            _ => self.items.as_span(),
        }
    }
}
