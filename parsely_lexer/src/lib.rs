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

        let token = match kw_slice {
            /* true */
            ['t', 'r', 'u', 'e'] => Some(Bool::from_value(true, slice, self.make_position())),
            /* false */
            ['f', 'a', 'l', 's', 'e'] => Some(Bool::from_value(false, slice, self.make_position())),

            /* Const */
            ['c', 'o', 'n', 's', 't'] => Some(Const::from_span_start(self.make_position())),
            /* Continue */
            ['c', 'o', 'n', 't', 'i', 'n', 'u', 'e'] => {
                Some(Continue::from_span_start(self.make_position()))
            }
            /* Break */
            ['b', 'r', 'e', 'a', 'k'] => Some(Break::from_span_start(self.make_position())),
            /* Else */
            ['e', 'l', 's', 'e'] => Some(Else::from_span_start(self.make_position())),
            /* Enum */
            ['e', 'n', 'u', 'm'] => Some(Enum::from_span_start(self.make_position())),
            /* Export */
            ['e', 'x', 'p', 'o', 'r', 't'] => Some(Export::from_span_start(self.make_position())),
            /* External */
            ['e', 'x', 't', 'e', 'r', 'n', 'a', 'l'] => {
                Some(External::from_span_start(self.make_position()))
            }
            /* For */ ['f', 'o', 'r'] => Some(For::from_span_start(self.make_position())),
            /* If */ ['i', 'f'] => Some(If::from_span_start(self.make_position())),
            /* Match */
            ['m', 'a', 't', 'c', 'h'] => Some(Match::from_span_start(self.make_position())),
            /* Nones */
            ['n', 'o', 'n', 'e'] => Some(Nones::from_span_start(self.make_position())),
            ['o', 'p', 'a', 'q', 'u', 'e'] => Some(Opaque::from_span_start(self.make_position())),
            ['p', 'a', 'c', 'k', 'e', 'd'] => Some(Packed::from_span_start(self.make_position())),
            /* Persist */
            ['p', 'e', 'r', 's', 'i', 's', 't'] => {
                Some(Persist::from_span_start(self.make_position()))
            }
            /* Return */
            ['r', 'e', 't', 'u', 'r', 'n'] => Some(Return::from_span_start(self.make_position())),
            /* Struct */
            ['s', 't', 'r', 'u', 'c', 't'] => Some(Struct::from_span_start(self.make_position())),
            /* Typedef */
            ['t', 'y', 'p', 'e', 'd', 'e', 'f'] => {
                Some(Typedef::from_span_start(self.make_position()))
            }
            /* Typeof */
            ['t', 'y', 'p', 'e', 'o', 'f'] => Some(Typeof::from_span_start(self.make_position())),
            /* Void */
            ['v', 'o', 'i', 'd'] => Some(Void::from_span_start(self.make_position())),
            /* While */
            ['w', 'h', 'i', 'l', 'e'] => Some(While::from_span_start(self.make_position())),
            c => Some(Ident::from_span_start(c, self.make_position())),
        };

        self.index += kw_slice.len();
        self.column += kw_slice.len();

        token
    }

    fn try_string(&mut self) -> Option<Token> {
        let char = *self.chars.get(self.index)?;
        match char {
            '"' => {
                let open = self.make_position();
                self.index += 1;

                let slice = self.chars.get(self.index..)?;

                let ind = slice.iter().position(|c| *c == '"').unwrap();

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
            ('"', _, _) => return Some(self.try_string()),

            // Punctuation
            (';', _, _) => Some(Semi::from_span_start(self.make_position())),
            (':', _, _) => Some(Colon::from_span_start(self.make_position())),
            (',', _, _) => Some(Comma::from_span_start(self.make_position())),
            ('#', _, _) => Some(Pound::from_span_start(self.make_position())),

            // Operators
            ('&', Some('='), _) => Some(AndEq::from_span_start(self.make_position())),
            ('&', Some('&'), _) => Some(LogicalAnd::from_span_start(self.make_position())),
            ('&', _, _) => Some(And::from_span_start(self.make_position())),
            ('=', Some('='), _) => Some(Eq::from_span_start(self.make_position())),
            ('=', _, _) => Some(Assign::from_span_start(self.make_position())),
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
    pub fn simple_test() {
        let input = r"
export struct data {
    int32 i = 8;
}

external void main() {

}
";

        let tokens = Lexer::run(input.as_bytes());
        println!("{:#?}", tokens);

        assert_eq!(
            vec![
                Token::Export(Export(span!(1:0-6))),
                Token::Struct(Struct(span!(1:7-13))),
                Token::Ident(Ident {
                    value: "data".to_string(),
                    span: span!(1:14-18)
                }),
                Token::Group(Group {
                    open: span!(1:19-20),
                    close: span!(3:0-1),
                    bracket: GroupBracket::Brace,
                    tokens: vec![
                        Token::Ident(Ident {
                            value: "int32".to_string(),
                            span: span!(2:4-9)
                        }),
                        Token::Ident(Ident {
                            value: "i".to_string(),
                            span: span!(2:10-11)
                        }),
                        Token::Assign(Assign(span!(2:12-13))),
                        Token::Int(Int {
                            value: 8,
                            span: span!(2:14-15)
                        }),
                        Token::Semi(Semi(span!(2:15-16))),
                    ]
                }),
                Token::External(External(span!(5:0-8))),
                Token::Void(Void(span!(5:9-13))),
                Token::Ident(Ident {
                    value: "main".to_string(),
                    span: span!(5:14-18)
                }),
                Token::Group(Group {
                    open: span!(5:18-19),
                    close: span!(5:19-20),
                    bracket: GroupBracket::Paren,
                    tokens: vec![]
                }),
                Token::Group(Group {
                    open: span!(5:21-22),
                    close: span!(7:0-1),
                    bracket: GroupBracket::Brace,
                    tokens: vec![]
                }),
            ],
            tokens.as_slice()
        );
    }
}
