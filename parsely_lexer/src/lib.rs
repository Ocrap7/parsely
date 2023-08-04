use tokens::*;

#[macro_use]
pub mod tokens;

/// Represents a single position in a source file
///
/// `line` line in the file (starting at 0)
/// `column` in the line (starting at 0)
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            end: Position {
                line: other.line,
                column: other.column + 1,
            },
        }
    }
}

/// Represents a range in a file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
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
            if let Some(token) = token {
                tokens.push(token)
            }
        }

        tokens.push(Token::Eof);

        tokens
    }

    /// Trys and parse keyword, ident, or number
    ///
    /// Returns `None` if the end of the buffer has been reached.
    ///
    fn try_keyword_or_ident(&mut self) -> Option<Token> {
        let slice = self.chars.get(self.index..)?;

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

        use parsely_macros::*;
        let token = match kw_slice {
            str_arr!("true") => Some(Bool::from_value(true, slice, self.make_position())),
            str_arr!("false") => Some(Bool::from_value(false, slice, self.make_position())),

            str_arr!("a") => consume_kw!(A),
            str_arr!("and") => consume_kw!(And),
            str_arr!("called") => consume_kw!(Called),
            str_arr!("constant") => consume_kw!(Constant),
            str_arr!("contains") => consume_kw!(Contains),
            str_arr!("define") => consume_kw!(Define),
            str_arr!("defines") => consume_kw!(Defines),
            str_arr!("executes") => consume_kw!(Executes),
            str_arr!("function") => consume_kw!(Function),
            str_arr!("of") => consume_kw!(Of),
            str_arr!("result") => consume_kw!(Result),
            str_arr!("starts") => consume_kw!(Starts),
            str_arr!("that") => consume_kw!(That),
            str_arr!("then") => consume_kw!(Then),
            str_arr!("the") => consume_kw!(The),
            str_arr!("value") => consume_kw!(Value),
            str_arr!("variable") => consume_kw!(Variable),
            str_arr!("with") => consume_kw!(With),

            _ => panic!("Unknown keyword: {}", kw_slice.iter().collect::<std::string::String>()),
        };


        self.index += kw_slice.len();
        self.column += kw_slice.len();

        token
    }

    fn try_string_or_ident(&mut self) -> Option<Token> {
        let char = *self.chars.get(self.index)?;
        match char {
            '\'' => {
                let open = self.make_position();
                self.index += 1;
                self.column += 1;

                let slice = self.chars.get(self.index..)?;

                let ind = slice.iter().position(|c| *c == '"').unwrap();

                let slice = &slice[..ind];
                self.index += slice.len();
                self.column += slice.len();

                let close = self.make_position();

                self.index += 1;
                self.column += 1;

                Some(Token::String(String {
                    value: slice.iter().collect(),
                    span: open.join(close),
                }))
            }
            '"' => {
                let open = self.make_position();
                self.index += 1;
                self.column += 1;

                let slice = self.chars.get(self.index..)?;

                let ind = slice.iter().position(|c| *c == '"').unwrap();

                let slice = &slice[..ind];
                self.index += slice.len();
                self.column += slice.len();

                let close = self.make_position();

                self.index += 1;
                self.column += 1;

                Some(Token::Ident(Ident {
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
        let char = *self.chars.get(self.index)?;
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
            ('"', _, _) => return Some(self.try_string_or_ident()),

            // Punctuation
            (';', _, _) => Some(Semi::from_span_start(self.make_position())),
            (':', _, _) => Some(Colon::from_span_start(self.make_position())),
            (',', _, _) => Some(Comma::from_span_start(self.make_position())),

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
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        let input = r#"
define a function called "x"
"#;
        let tokens = Lexer::run(input);

        assert_eq!(tokens,
            vec![
                Token::Define(Define(span!(1:0-6))),
                Token::A(A(span!(1:7-8))),
                Token::Function(Function(span!(1:9-17))),
                Token::Called(Called(span!(1:18-24))),
                Token::Ident(Ident {
                    value: "x".into(),
                    span: span!(1:25-28),
                })
            ]
        )
    }
}