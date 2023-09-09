use std::{collections::HashMap, path::Path};

use parsely_lexer::{tokens::Token, AsSpan, Position, Range, Span};

use crate::{item::Item, tokens, ParseStream};

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
    pub items: Vec<Item>,
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
            // assert!(self.source.contains('\n'));
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

        let (end_index, _) = self
            .source
            .match_indices('\n')
            .nth(span.end.line)
            .unwrap_or((self.source.len(), ""));
        let end_index = end_index - self.line_ending().as_str().len() + 1; // Get the char after the linefeed

        Some(&self.source[start_index..end_index])
    }

    /// Returns a slice of tokens based on the token range
    pub fn tokens(&self, range: impl Into<Range>) -> &[Token] {
        let range: Range = range.into();
        &self.tokens[range.0.start..range.0.end]
    }
}