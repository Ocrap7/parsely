#![feature(result_option_inspect)]
use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
};

use parsely_macros::{consume_kw, match_token};
use tokens::*;

#[macro_use]
pub mod tokens;

pub trait AsSpan {
    fn as_span(&self) -> Span;
}

impl<T: AsSpan> AsSpan for Option<T> {
    fn as_span(&self) -> Span {
        self.as_ref().map(AsSpan::as_span).unwrap_or_default()
    }
}

pub trait SearchPosition<T>
where
    T: AsSpan,
{
    fn search_position(&self, position: &Position) -> Option<usize>;
}

impl<T: AsSpan + Debug> SearchPosition<T> for [T] {
    fn search_position(&self, position: &Position) -> Option<usize> {
        self.binary_search_by(|item| {
            let span = item.as_span();

            if position < &span.start {
                Ordering::Greater
            } else if position > &span.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        })
        .ok()
    }
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
    pub const EMPTY: Position = Position { line: 0, column: 0 };

    pub fn increment_column(self) -> Self {
        Position {
            line: self.line,
            column: self.column + 1,
        }
    }

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

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}-{}:{}",
            self.start.line, self.start.column, self.end.line, self.end.column
        )
    }
}

impl Span {
    pub const EMPTY: Span = Span {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    };

    pub fn new_dummy(position: Position) -> Span {
        Span {
            start: position,
            end: position,
        }
    }

    pub fn increment(self) -> Self {
        Span {
            start: Position {
                line: self.start.line,
                column: self.start.column + 1,
            },
            end: Position {
                line: self.end.line,
                column: self.end.column + 1,
            },
        }
    }

    /// Length of span on a single line
    pub fn len(&self) -> usize {
        self.end.column - self.start.column
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_dummy(&self) -> bool {
        self.start == self.end
    }

    pub fn contains(&self, other: &Span) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    pub fn contains_position(&self, position: &Position) -> bool {
        self.start <= *position && self.end >= *position
    }

    pub fn join_all<const N: usize>(spans: [Span; N]) -> Span {
        assert_ne!(N, 0);
        assert_ne!(N, 1);

        let start_iter = spans.into_iter().skip_while(|span| span == &Span::EMPTY);

        let mut rest = start_iter.take_while(|span| span != &Span::EMPTY);

        let first = rest.next();
        let last = rest.last().or_else(|| first);

        first.unwrap().join(last.unwrap())
    }

    pub fn join_saturated(self, other: Span) -> Span {
        if self == Span::EMPTY {
            other
        } else if other == Span::EMPTY {
            self
        } else {
            self.join(other)
        }
    }

    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
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
            match_token!["true"] => Some(Bool::from_value(true, slice, self.make_position())),
            match_token!["false"] => Some(Bool::from_value(false, slice, self.make_position())),

            match_token!["let"] => consume_kw![Let],
            match_token!["mut"] => consume_kw![Mut],
            match_token!["type"] => consume_kw![Type],
            match_token!["module"] => consume_kw![Module],
            match_token!["if"] => consume_kw![If],
            match_token!["then"] => consume_kw![Then],
            match_token!["else"] => consume_kw![Else],
            match_token!["loop"] => consume_kw![Loop],
            match_token!["do"] => consume_kw![Do],
            match_token!["of"] => consume_kw![Of],
            match_token!["in"] => consume_kw![In],
            match_token!["none"] => consume_kw![Nones],
            match_token!["match"] => consume_kw![Match],
            match_token!["with"] => consume_kw![With],
            match_token!["export"] => consume_kw![Export],
            match_token!["import"] => consume_kw![Import],
            match_token!["const"] => consume_kw![Const],
            match_token!["inline"] => consume_kw![Inline],
            match_token!["internal"] => consume_kw![Internal],
            match_token!["persist"] => consume_kw![Persist],
            match_token!["return"] => consume_kw![Return],
            match_token!["break"] => consume_kw![Break],
            match_token!["continue"] => consume_kw![Continue],

            c => Some(Ident::from_span_start(c, self.make_position())),
        };

        self.index += kw_slice.len();
        self.column += kw_slice.len();

        token
    }

    fn try_numeric(&mut self) -> Option<Token> {
        let Some(slice) = self.chars.get(self.index..) else {
            let mut pos = self.make_position();
            pos.column -= 2;

            return Some(Token::Eof(pos.join(pos)));
        };

        let int_ind = slice
            .iter()
            .position(|c| !c.is_ascii_digit())
            .unwrap_or(slice.len());

        let mut dotted = false;
        let float_ind = slice
            .iter()
            .position(|c| {
                let res = !c.is_ascii_digit() && *c != '.' || dotted;

                if *c == '.' {
                    dotted = true
                }

                res
            })
            .unwrap_or(slice.len());

        let int_slice = &slice[..int_ind];
        let float_slice = &slice[..float_ind];

        // We check if there is a duplicate '.' in case of an interger range: 0..10
        if !float_slice.is_empty()
            && float_slice.contains(&'.')
            && slice
                .get(float_ind - 1..=float_ind)
                .inspect(|f| println!("{f:?}"))
                .filter(|c| if let ['.', '.'] = c { true } else { false })
                .is_none()
        {
            let token = Some(Float::from_span_start(float_slice, self.make_position()));

            self.index += float_slice.len();
            self.column += float_slice.len();

            return token;
        }

        if !int_slice.is_empty() {
            let token = Some(Int::from_span_start(int_slice, self.make_position()));

            self.index += int_slice.len();
            self.column += int_slice.len();

            return token;
        }

        None
    }

    fn try_string(&mut self) -> Option<Token> {
        let char = *self.chars.get(self.index)?;
        match char {
            '"' | '\'' => {
                let open = self.make_position();
                self.index += 1;
                self.column += 1;

                let slice = self.chars.get(self.index..)?;

                let ind = slice.iter().position(|c| *c == char).unwrap();

                let slice = &slice[..ind];

                self.index += slice.len() + 1;
                self.column += slice.len() + 1;

                let close = self.make_position();

                Some(Token::String(String {
                    value: slice.iter().collect(),
                    span: open.join(close),
                }))
            }
            _ => None,
        }
    }

    fn try_template(&mut self) -> Option<Token> {
        let open = self.make_position();
        self.index += 1;
        self.column += 1;

        let char = *self.chars.get(self.index)?;

        match char {
            '{' => {
                self.index += 1;
                self.column += 1;

                let mut acc = 0;
                let start = self.index;

                loop {
                    match self.chars.get(self.index) {
                        Some('{') => acc += 1,
                        Some('}') if acc == 0 => break,
                        Some('}') => acc -= 1,
                        Some('\n') => {
                            self.index += 1;
                            self.column = 0;
                            self.line += 1;

                            continue;
                        }
                        None => return None,
                        _ => (),
                    }

                    self.index += 1;
                    self.column += 1;
                }

                let slice = self.chars.get(start..self.index)?;

                self.index += 1;
                self.column += 1;
                let close = self.make_position();

                Some(Token::Template(Template {
                    value: slice.iter().collect(),
                    span: open.join(close),
                }))
            }
            _ => None,
        }
    }

    fn line_comment(&mut self) -> Option<Option<Token>> {
        let chars = [self.chars.get(self.index)?, self.chars.get(self.index + 1)?];

        match chars {
            ['/', '/'] => {
                self.index += 2;

                while self.index < self.chars.len() {
                    if let Some('\n') = self.chars.get(self.index) {
                        break;
                    }

                    self.index += 1;
                }

                self.index += 1;
                self.column = 0;
                self.line += 1;

                Some(None)
            }
            _ => None,
        }
    }

    fn block_comment(&mut self) -> Option<Option<Token>> {
        let chars = [self.chars.get(self.index)?, self.chars.get(self.index + 1)?];

        match chars {
            ['/', '*'] => {
                self.index += 2;
                self.column += 2;

                while self.index < self.chars.len() {
                    let chars = [self.chars.get(self.index)?, self.chars.get(self.index + 1)?];

                    match chars {
                        ['*', '/'] => {
                            self.index += 2;
                            self.column += 2;

                            return Some(None);
                        }
                        _ => (),
                    }

                    self.index += 1;
                    self.column += 1;
                }

                Some(None)
            }
            _ => None,
        }
    }

    fn doc_comment(&mut self) -> Option<Option<Token>> {
        let chars = [
            self.chars.get(self.index)?,
            self.chars.get(self.index + 1)?,
            self.chars.get(self.index + 2)?,
        ];

        let start = self.make_position();

        match chars {
            ['/', '/', '/'] => {
                self.index += 3;
                self.column += 3;

                let start_value_index = self.index;
                let start_value = self.make_position();

                while self.index < self.chars.len() {
                    if let Some('\n') = self.chars.get(self.index) {
                        break;
                    }

                    self.index += 1;
                }

                let end = self.make_position();
                self.index += 1;

                self.line += 1;
                self.column = 0;

                let slice = &self.chars[start_value_index..self.index];

                let token = DocComment {
                    span: start.join(end),
                    value_span: start_value.join(end),
                    value: slice
                        .iter()
                        .collect::<std::string::String>()
                        .trim()
                        .to_string(),
                };

                Some(Some(Token::DocComment(token)))
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
            // pos.column -= 1;

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
            ('a'..='z' | 'A'..='Z', _, _) => return Some(self.try_keyword_or_ident()),
            ('0'..='9', _, _) => return Some(self.try_numeric()),
            ('"' | '\'', _, _) => return Some(self.try_string()),
            ('/', Some('/'), Some('/')) => return self.doc_comment(),
            ('/', Some('/'), _) => return self.line_comment(),
            ('/', Some('*'), _) => return self.block_comment(),

            // Punctuation
            (';', _, _) => Some(Semi::from_span_start(self.make_position())),
            ('?', _, _) => Some(Question::from_span_start(self.make_position())),
            (':', _, _) => Some(Colon::from_span_start(self.make_position())),
            (',', _, _) => Some(Comma::from_span_start(self.make_position())),
            ('#', Some('!'), _) => Some(Shebang::from_span_start(self.make_position())),
            ('#', _, _) => Some(Pound::from_span_start(self.make_position())),

            // Operators
            ('&', Some('='), _) => Some(AndEq::from_span_start(self.make_position())),
            ('&', Some('&'), _) => Some(LogicalAnd::from_span_start(self.make_position())),
            ('&', _, _) => Some(And::from_span_start(self.make_position())),
            ('=', Some('='), _) => Some(Eq::from_span_start(self.make_position())),
            ('=', Some('>'), _) => Some(Arrow::from_span_start(self.make_position())),
            ('=', _, _) => Some(Assign::from_span_start(self.make_position())),
            ('.', Some('.'), Some('=')) => Some(RangeEq::from_span_start(self.make_position())),
            ('.', Some('.'), _) => Some(tokens::Range::from_span_start(self.make_position())),
            ('.', _, _) => Some(Dot::from_span_start(self.make_position())),
            ('>', Some('>'), Some('=')) => {
                Some(RightShiftEq::from_span_start(self.make_position()))
            }
            ('>', Some('>'), _) => Some(RightShift::from_span_start(self.make_position())),
            ('>', Some('='), _) => Some(GtEq::from_span_start(self.make_position())),
            ('>', _, _) => Some(Gt::from_span_start(self.make_position())),
            ('<', Some('<'), Some('=')) => Some(LeftShiftEq::from_span_start(self.make_position())),
            ('<', Some('<'), _) => Some(LeftShift::from_span_start(self.make_position())),
            ('|', Some('|'), _) => Some(LogicalOr::from_span_start(self.make_position())),
            ('|', Some('='), _) => Some(OrEq::from_span_start(self.make_position())),
            ('|', _, _) => Some(Or::from_span_start(self.make_position())),
            ('<', Some('='), _) => Some(LtEq::from_span_start(self.make_position())),
            ('<', _, _) => Some(Lt::from_span_start(self.make_position())),
            ('-', Some('='), _) => Some(MinusEq::from_span_start(self.make_position())),
            ('-', _, _) => Some(Minus::from_span_start(self.make_position())),
            ('!', Some('='), _) => Some(NotEq::from_span_start(self.make_position())),
            ('!', _, _) => Some(Not::from_span_start(self.make_position())),
            ('+', Some('='), _) => Some(PlusEq::from_span_start(self.make_position())),
            ('+', _, _) => Some(Plus::from_span_start(self.make_position())),
            ('%', Some('='), _) => Some(RemEq::from_span_start(self.make_position())),
            ('%', _, _) => Some(Rem::from_span_start(self.make_position())),
            ('/', Some('='), _) => Some(SlashEq::from_span_start(self.make_position())),
            ('/', _, _) => Some(Slash::from_span_start(self.make_position())),
            ('*', Some('='), _) => Some(StarEq::from_span_start(self.make_position())),
            ('*', _, _) => Some(Star::from_span_start(self.make_position())),
            ('^', Some('='), _) => Some(XorEq::from_span_start(self.make_position())),
            ('^', _, _) => Some(Xor::from_span_start(self.make_position())),

            // Whitespace
            ('\r', Some('\n'), _) => {
                let span = Span {
                    start: Position {
                        line: self.line,
                        column: self.column,
                    },
                    end: Position {
                        line: self.line,
                        column: self.column + 2,
                    },
                };

                self.index += 2;
                self.column = 0;
                self.line += 1;

                // Some(Token::Newline(Newline { span }))
                None
            }
            ('\n', _, _) => {
                let span = Span {
                    start: Position {
                        line: self.line,
                        column: self.column,
                    },
                    end: Position {
                        line: self.line,
                        column: self.column + 1,
                    },
                };

                self.index += 1;
                self.column = 0;
                self.line += 1;

                // Some(Token::Newline(Newline { span }))
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
