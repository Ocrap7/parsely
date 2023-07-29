use std::arch::asm;

use tokens::*;

pub mod tokens;

/// Represents a single position in a source file
/// 
/// `line` line in the file (starting at 0)
/// `column` in the line (starting at 0)
/// 
#[derive(Debug, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

/// Represents a range in a file
#[derive(Debug, PartialEq, Eq)]
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
    /// Length of span on a single line
    pub fn len(&self) -> usize {
        self.end.column - self.start.column
    }
}

/// Main lexing struct. This is an iterator
/// 
/// ## Example
/// 
/// ```ignore
/// use parsely_lexer::{span, Lexer, tokens::*};
/// 
/// let input = r"int32 i;";
/// 
/// let mut tokens = Lexer::run(input.as_bytes());
/// 
/// assert_eq!(
///     tokens.next(),
///     Some(Token::Ident(Ident {
///         value: "int32".to_string(),
///         span: span!(0:0-5)
///     }))
/// );
/// assert_eq!(
///     tokens.next(),
///     Some(Token::Ident(Ident {
///         value: "i".to_string(),
///         span: span!(0:6-7)
///     }))
/// );
/// assert_eq!(tokens.next(), Some(Token::Semi(Semi(span!(0:7-8)))));
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
    pub fn run(buffer: impl AsRef<[u8]>) -> impl Iterator<Item = Token> {
        let str = std::str::from_utf8(buffer.as_ref()).expect("Unable to decode buffer as utf8!");

        let lexer = Lexer {
            line: 0,
            column: 0,
            index: 0,
            chars: str.chars().collect(),
        };

        lexer.filter_map(|c| c)
    }

    /// Returns a position from the current state of the lexer.
    fn make_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }

    /// Trys and parse keyword, ident, or number
    /// 
    /// Returns `None` if the end of the buffer has been reached. 
    /// 
    fn try_keyword_or_ident(&mut self) -> Option<Token> {
        let slice = self.chars.get(self.index..)?;

        let int_ind = slice
            .iter()
            .position(|c| !c.is_digit(10))
            .unwrap_or(slice.len());

        let float_ind = slice
            .iter()
            .position(|c| !c.is_digit(10))
            .unwrap_or(slice.len());

        let kw_ind = slice
            .iter()
            .position(|c| !c.is_alphanumeric() && *c != '_')
            .unwrap_or(slice.len());

        let int_slice = &slice[..int_ind];
        let float_slice = &slice[..float_ind];
        let kw_slice = &slice[..kw_ind];

        if int_slice.len() > 0 {
            let token = Some(Int::from_span_start(int_slice, self.make_position()));

            self.index += int_slice.len();
            self.column += int_slice.len();

            return token;
        }

        if float_slice.len() > 0 {
            let token = Some(Float::from_span_start(float_slice, self.make_position()));

            self.index += float_slice.len();
            self.column += float_slice.len();

            return token;
        }

        unsafe { asm!("nop; hlt"); }
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
}

impl Iterator for Lexer {
    type Item = Option<Token>;

    /// Try and get next token. 
    /// 
    /// If a valid token was found, `Some` is returned. Otherwise, if no tokens are left, `None` is returned.
    /// 
    /// White space is ignored
    fn next(&mut self) -> Option<Self::Item> {
        let char = self.chars.get(self.index)?;
        let char_1 = self.chars.get(self.index + 1);
        let char_2 = self.chars.get(self.index + 2);

        let token = match (char, char_1, char_2) {
            // Keywords and identifiers
            ('a'..='z' | '0'..='9', _, _) => return Some(self.try_keyword_or_ident()),

            // Punctuation
            (';', _, _) => Some(Semi::from_span_start(self.make_position())),
            (':', _, _) => Some(Colon::from_span_start(self.make_position())),
            (',', _, _) => Some(Comma::from_span_start(self.make_position())),
            ('"', _, _) => Some(Quote::from_span_start(self.make_position())),
            ('#', _, _) => Some(Pound::from_span_start(self.make_position())),
            ('\'', _, _) => Some(SingleQuote::from_span_start(self.make_position())),
            ('{', _, _) => Some(OpenBrace::from_span_start(self.make_position())),
            ('}', _, _) => Some(CloseBrace::from_span_start(self.make_position())),
            ('[', _, _) => Some(OpenBracket::from_span_start(self.make_position())),
            (']', _, _) => Some(CloseBracket::from_span_start(self.make_position())),
            ('(', _, _) => Some(OpenParen::from_span_start(self.make_position())),
            (')', _, _) => Some(CloseParen::from_span_start(self.make_position())),

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
            _ => panic!("Unknown token `{}`", char),
        };

        token.as_ref().map(|tok| {
            self.column += tok.len();
            self.index += tok.len();
        });

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

        let mut tokens = Lexer::run(input.as_bytes());

        assert_eq!(tokens.next(), Some(Token::Export(Export(span!(1:0-6)))));
        assert_eq!(tokens.next(), Some(Token::Struct(Struct(span!(1:7-13)))));
        assert_eq!(
            tokens.next(),
            Some(Token::Ident(Ident {
                value: "data".to_string(),
                span: span!(1:14-18)
            }))
        );
        assert_eq!(tokens.next(), Some(Token::OpenBrace(OpenBrace(span!(1:19-20)))));
        assert_eq!(
            tokens.next(),
            Some(Token::Ident(Ident {
                value: "int32".to_string(),
                span: span!(2:4-9)
            }))
        );
        assert_eq!(
            tokens.next(),
            Some(Token::Ident(Ident {
                value: "i".to_string(),
                span: span!(2:10-11)
            }))
        );
        assert_eq!(tokens.next(), Some(Token::Assign(Assign(span!(2:12-13)))));
        assert_eq!(
            tokens.next(),
            Some(Token::Int(Int {
                value: 8,
                span: span!(2:14-15)
            }))
        );
        assert_eq!(tokens.next(), Some(Token::Semi(Semi(span!(2:15-16)))));
        assert_eq!(tokens.next(), Some(Token::CloseBrace(CloseBrace(span!(3:0-1)))));
        assert_eq!(tokens.next(), Some(Token::External(External(span!(5:0-8)))));
        assert_eq!(tokens.next(), Some(Token::Void(Void(span!(5:9-13)))));
        assert_eq!(
            tokens.next(),
            Some(Token::Ident(Ident {
                value: "main".to_string(),
                span: span!(5:14-18)
            }))
        );
        assert_eq!(tokens.next(), Some(Token::OpenParen(OpenParen(span!(5:18-19)))));
        assert_eq!(tokens.next(), Some(Token::CloseParen(CloseParen(span!(5:19-20)))));
        assert_eq!(tokens.next(), Some(Token::OpenBrace(OpenBrace(span!(5:21-22)))));
        assert_eq!(tokens.next(), Some(Token::CloseBrace(CloseBrace(span!(7:0-1)))));
    }
}
