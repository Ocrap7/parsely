use std::{collections::HashMap, path::Path};

use parsely_lexer::{
    tokens::{self, Token},
    AsSpan, Position, Span,
};
use parsely_macros::AsSpan;

use crate::{statement::Init, types, Parse, ParseError, ParseStream, Punctuation};

#[derive(Debug, Clone, AsSpan)]
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

#[derive(Debug, Clone, AsSpan)]
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

#[derive(Debug, Clone, AsSpan)]
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

#[derive(Debug, Clone, AsSpan)]
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

#[derive(Debug, Clone, AsSpan)]
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

#[derive(Debug, Clone, AsSpan)]
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

#[derive(Debug, Clone, AsSpan)]
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

pub enum LineEnding {
    LF,
    CRLF,
}

impl LineEnding {
    pub fn as_str(&self) -> &str {
        match self {
            LineEnding::LF => "\n",
            LineEnding::CRLF => "\r\n",
        }
    }
}

/// This represents a parsed file
#[derive(Debug, Clone)]
pub struct Program {
    /// Path to the source file
    pub path: std::path::PathBuf,
    /// Source code string
    pub source: String,
    /// Lexed tokens
    pub tokens: Vec<Token>,

    /// Parsed items
    pub items: Vec<TopLevelItem>,
    /// If this file contains errors
    pub dirty: bool,
}

impl Program {
    /// Creates a new program, ready to be parsed
    pub fn new(path: impl AsRef<Path>, source: String, tokens: Vec<Token>) -> Program {
        Program {
            path: path.as_ref().to_path_buf(),
            source,
            tokens,

            items: Vec::new(),
            dirty: false,
        }
    }

    /// Parses the program, filling the `items` field with parsed values
    pub fn parse(mut self) -> crate::Result<Program> {
        let stream = ParseStream::from(&self.tokens);
        self.items = stream.parse()?;
        Ok(self)
    }

    /// Determines the line endings this file uses.
    ///
    /// If CRLF is not detected, LF is assumed
    pub fn line_ending(&self) -> LineEnding {
        if self.source.contains("\r\n") {
            LineEnding::CRLF
        } else {
            assert!(self.source.contains('\n'));
            LineEnding::LF
        }
    }

    /// Get a slice into the `source` field using `span`.
    /// This will work for multi-line spans and will slice into the columns.
    pub fn source_slice(&self, span: &Span) -> Option<&str> {
        let start_index = if span.start.line == 0 {
            0
        } else {
            self.source.match_indices('\n').nth(span.start.line - 1)?.0
        };
        let start_index = start_index + 1; // Get the char after the linefeed

        let (end_index, _) = self.source.match_indices('\n').nth(span.end.line - 1)?;
        let end_index = end_index - self.line_ending().as_str().len() + 1; // Get the char after the linefeed

        Some(&self.source[start_index + span.start.column..end_index + span.end.column + 1])
    }

    /// Get a slice into the `source` field using `span`.
    /// This will work for multi-line spans, but will ignore columns (returns full lines).
    pub fn source_lines_slice(&self, span: &Span) -> Option<&str> {
        let start_index = if span.start.line == 0 {
            0
        } else {
            self.source.match_indices('\n').nth(span.start.line - 1)?.0
        };
        let start_index = start_index + 1; // Get the char after the linefeed

        let (end_index, _) = self.source.match_indices('\n').nth(span.end.line)?;
        let end_index = end_index - self.line_ending().as_str().len() + 1; // Get the char after the linefeed

        Some(&self.source[start_index..end_index])
    }

    /// Returns a slice of tokens based on the token range
    pub fn tokens(&self, range: impl Into<tokens::Range>) -> &[Token] {
        let range: tokens::Range = range.into();
        &self.tokens[range.0.start..range.0.end]
    }
}

impl parsely_lexer::AsSpan for Program {
    fn as_span(&self) -> parsely_lexer::Span {
        self.items.as_span()
    }
}

pub struct TokenCache<'a> {
    program: &'a Program,
    pub(crate) positions: HashMap<Position, usize>,
    pub(crate) lines: HashMap<usize, tokens::Range>,
}

impl<'a> TokenCache<'a> {
    pub fn new(program: &'a Program) -> TokenCache<'a> {
        TokenCache {
            program,
            positions: HashMap::default(),
            lines: HashMap::new(),
        }
    }

    pub fn token_index(&mut self, position: &Position) -> usize {
        if let Some(position) = self.positions.get(position) {
            *position
        } else {
            let token_range = self.line_index(position.line);
            let start_index = token_range.0.start;

            let tokens = self.program.tokens(token_range);

            let token_index = tokens.iter().enumerate().position(|(i, tok)| {
                self.positions.insert(tok.as_span().start, start_index + i);

                tok.as_span().start == *position
            });

            start_index + token_index.expect("Unable to find token with position")
        }
    }

    pub fn line_index(&mut self, line: usize) -> tokens::Range {
        if let Some(range) = self.lines.get(&line) {
            range.clone()
        } else {
            let line_token_index = self
                .program
                .tokens
                .binary_search_by_key(&line, |tok| tok.as_span().start.line)
                .expect("Unable to find token with specified line number");

            let start_index = self
                .program
                .tokens
                .iter()
                .take(line_token_index)
                .rev()
                .position(|tok| tok.as_span().start.line != line)
                .unwrap_or(0);
            let start_index = line_token_index - start_index;

            let tokens = &self.program.tokens[start_index..];
            let end_index = tokens
                .iter()
                .position(|tok| tok.as_span().start.line != line)
                .unwrap_or(tokens.len());

            self.lines
                .insert(line, tokens::Range(start_index..start_index + end_index));

            tokens::Range(start_index..start_index + end_index)
        }
    }
}
