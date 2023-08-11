use tokens::*;

#[macro_use]
pub mod tokens;

pub trait AsSpan {
    fn as_span(&self) -> Span;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Range(pub std::ops::Range<usize>);

/// Represents a single position in a source file
///
/// `line` line in the file (starting at 0)
/// `column` in the line (starting at 0)
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn to_span(self, len: usize) -> Span {
        Span {
            end: Position {
                line: self.line,
                column: self.column + len,
            },
            start: self,
        }
    }

    pub fn join(self, other: Position) -> Span {
        Span {
            start: self,
            end: other,
        }
    }
}

/// Represents a range in a file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Default for Span {
    fn default() -> Self {
        Span::EMPTY
    }
}

impl<T: AsSpan> AsSpan for &T {
    fn as_span(&self) -> Span {
        AsSpan::as_span(*self)
    }
}

impl<T: AsSpan> AsSpan for Box<T> {
    fn as_span(&self) -> Span {
        self.as_ref().as_span()
    }
}

impl<L, R> From<(L, R)> for Span
where
    L: AsSpan,
    R: AsSpan,
{
    fn from(value: (L, R)) -> Self {
        Span {
            start: value.0.as_span().start,
            end: value.1.as_span().end,
        }
    }
}

impl AsSpan for (Span, Span) {
    fn as_span(&self) -> Span {
        Span {
            start: self.0.start,
            end: self.1.end,
        }
    }
}

impl AsSpan for (Position, Position) {
    fn as_span(&self) -> Span {
        Span {
            start: self.0,
            end: self.1,
        }
    }
}

impl<T: AsSpan> AsSpan for Vec<T> {
    fn as_span(&self) -> Span {
        let first = self.first().unwrap();
        let last = self.last().unwrap();
        first.as_span().join(last.as_span())
    }
}

// impl <T: Into<Span>> AsSpan for T {
//     fn as_span(&self) -> Span {
//         self.into()
//     }
// }

impl<L: AsSpan, R: AsSpan> AsSpan for (L, R) {
    fn as_span(&self) -> Span {
        Span {
            start: self.0.as_span().start,
            end: self.1.as_span().end,
        }
    }
}

#[macro_export]
macro_rules! span {
    ($line:literal:$s:literal-$e:literal) => {
        $crate::Span {
            start: $crate::Position {
                line: $line,
                column: $s,
            },
            end: $crate::Position {
                line: $line,
                column: $e,
            },
        }
    };
}

impl Span {
    pub const EMPTY: Span = Span {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    };

    /// Length of span on a single line
    pub fn len(&self) -> usize {
        self.end.column - self.start.column
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn contains(&self, other: &Span) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    pub fn contains_position(&self, position: &Position) -> bool {
        self.start <= *position && self.end >= *position
    }

    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

/// Main lexing struct. This is an iterator
///
/// ## Example
///
/// ```ignore
/// use parsely_lexer::{span, Lexer, tokens::*};
///
/// let input = "int32 i;";
///
/// let tokens = Lexer::run(input.as_bytes());
///
/// assert_eq!(
///     &[
///         Token::Ident(Ident {
///             value: "int32".to_string(),
///             span: span!(0:0-5)
///         }),
///         Token::Ident(Ident {
///             value: "i".to_string(),
///             span: span!(0:6-7)
///         }),
///         Token::Semi(Semi(span!(0:7-8))),
///     ],
///     tokens.as_slice(),
/// );
/// ```
pub struct Lexer {
    line: usize,
    column: usize,
    index: usize,
    chars: Vec<char>,
}

impl Lexer {
    /// Returns a `Lexer` as an iterator
    ///
    /// `buffer` should be a u8 ref
    pub fn run(buffer: impl AsRef<[u8]>) -> Vec<Token> {
        let str = std::str::from_utf8(buffer.as_ref()).expect("Unable to decode buffer as utf8!");

        let mut lexer = Lexer {
            line: 0,
            column: 0,
            index: 0,
            chars: str.chars().collect(),
        };

        lexer.collect(None)
    }

    /// Returns a position from the current state of the lexer.
    fn make_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    fn collect(&mut self, group: Option<GroupBracket>) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next(group) {
            match token {
                Some(tok @ Token::Eof(_)) => {
                    tokens.push(tok);
                    break;
                }
                Some(tok) => tokens.push(tok),
                None => (),
            }
        }

        tokens
    }

    /// Trys and parse keyword, ident, or number
    ///
    /// Returns `None` if the end of the buffer has been reached.
    ///
    fn try_keyword_or_ident(&mut self) -> Option<Token> {
        let Some(slice) = self.chars.get(self.index..) else {
            let mut pos = self.make_position();
            pos.column -= 2;

            return Some(Token::Eof(pos.join(pos)));
        };

        let int_ind = slice
            .iter()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(slice.len());

        let float_ind = slice
            .iter()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(slice.len());

        let kw_ind = slice
            .iter()
            .position(|c| !c.is_alphanumeric() && *c != '_')
            .unwrap_or(slice.len());

        let int_slice = &slice[..int_ind];
        let float_slice = &slice[..float_ind];
        let kw_slice = &slice[..kw_ind];

        if !int_slice.is_empty() {
            let token = Some(Int::from_span_start(int_slice, self.make_position()));

            self.index += int_slice.len();
            self.column += int_slice.len();

            return token;
        }

        if !float_slice.is_empty() {
            let token = Some(Float::from_span_start(float_slice, self.make_position()));

            self.index += float_slice.len();
            self.column += float_slice.len();

            return token;
        }

        let token = match kw_slice {
            /* true */
            ['t', 'r', 'u', 'e'] => Some(Bool::from_value(true, slice, self.make_position())),
            /* false */
            ['f', 'a', 'l', 's', 'e'] => Some(Bool::from_value(false, slice, self.make_position())),
            c => Some(Ident::from_span_start(c, self.make_position())),
        };

        self.index += kw_slice.len();
        self.column += kw_slice.len();

        token
    }

    fn try_string(&mut self) -> Option<Token> {
        let char = *self.chars.get(self.index)?;
        match char {
            '"' | '\'' => {
                let open = self.make_position();
                self.index += 1;

                let slice = self.chars.get(self.index..)?;

                let ind = slice.iter().position(|c| *c == char).unwrap();

                let close = self.make_position();
                let slice = &slice[..ind];

                self.index += slice.len() + 1;
                self.column += slice.len() + 1;

                Some(Token::String(String {
                    value: slice.iter().collect(),
                    span: open.join(close),
                }))
            }
            _ => None,
        }
    }
}

impl Lexer {
    /// Try and get next token.
    ///
    /// If a valid token was found, `Some` is returned. Otherwise, if no tokens are left, `None` is returned.
    ///
    /// White space is ignored
    fn next(&mut self, group: Option<GroupBracket>) -> Option<Option<Token>> {
        let Some(char) = self.chars.get(self.index) else {
            let mut pos = self.make_position();
            println!("{pos:?}");
            pos.column -= 1;

            return Some(Some(Token::Eof(pos.join(pos))));
        };

        let char_1 = self.chars.get(self.index + 1).copied();
        let char_2 = self.chars.get(self.index + 2).copied();

        match (char, group) {
            ('(', _) => {
                let open = self.make_position().to_span(1);
                self.index += 1;
                self.column += 1;

                let tokens = self.collect(Some(GroupBracket::Paren));

                let close = self.make_position().to_span(1);
                self.index += 1;
                self.column += 1;

                return Some(Some(Token::Group(Group {
                    open,
                    close,
                    bracket: GroupBracket::Paren,
                    tokens,
                })));
            }
            (')', Some(GroupBracket::Paren)) => return None,
            ('[', _) => {
                let open = self.make_position().to_span(1);
                self.index += 1;
                self.column += 1;

                let tokens = self.collect(Some(GroupBracket::Bracket));

                let close = self.make_position().to_span(1);
                self.index += 1;
                self.column += 1;

                return Some(Some(Token::Group(Group {
                    open,
                    close,
                    bracket: GroupBracket::Bracket,
                    tokens,
                })));
            }
            (']', Some(GroupBracket::Bracket)) => return None,
            ('{', _) => {
                let open = self.make_position().to_span(1);
                self.index += 1;
                self.column += 1;

                let tokens = self.collect(Some(GroupBracket::Brace));

                let close = self.make_position().to_span(1);
                self.index += 1;
                self.column += 1;

                return Some(Some(Token::Group(Group {
                    open,
                    close,
                    bracket: GroupBracket::Brace,
                    tokens,
                })));
            }
            ('}', Some(GroupBracket::Brace)) => return None,
            _ => (),
        }

        let token = match (char, char_1, char_2) {
            // Keywords and identifiers
            ('a'..='z' | '0'..='9', _, _) => return Some(self.try_keyword_or_ident()),
            ('"' | '\'', _, _) => return Some(self.try_string()),

            // Punctuation
            (';', _, _) => Some(Semi::from_span_start(self.make_position())),
            (':', _, _) => Some(Colon::from_span_start(self.make_position())),
            (',', _, _) => Some(Comma::from_span_start(self.make_position())),
            ('#', _, _) => Some(Pound::from_span_start(self.make_position())),

            // Whitespace
            ('\r', Some('\n'), _) => {
                self.index += 2;
                self.column = 0;
                self.line += 1;

                None
            }
            ('\n', _, _) => {
                self.index += 1;
                self.column = 0;
                self.line += 1;

                None
            }
            (' ' | '\x09'..='\x0d', _, _) => {
                self.index += 1;
                self.column += 1;

                None
            }
            _ => panic!("Unknown token `{char}`"),
        };

        if let Some(tok) = token.as_ref() {
            self.column += tok.len();
            self.index += tok.len();
        }

        Some(token)
    }
}

#[cfg(test)]
mod tests {}
