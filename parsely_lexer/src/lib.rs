use std::str::Chars;

pub mod tokens;

#[derive(Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Span {
    start: Position,
    end: Position,
}

define_punctuation! {
    pub enum Token {
        Plus = "+",
        Minus = "-",
        Star = "*",
        Slash = "/",
    }
}

pub struct Lexer<'a> {
    line: usize,
    column: usize,
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn run(buffer: &'a [u8]) -> impl Iterator<Item = Token> + 'a {
        let str = std::str::from_utf8(buffer.as_ref()).expect("Unable to decode buffer as utf8!");

        let lexer = Lexer {
            line: 0,
            column: 0,
            chars: str.chars(),
        };

        lexer.filter_map(|c| c)
    }

    fn make_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.column,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Option<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let char = self.chars.next()?;

        let token = match char {
            '+' => Some(Plus::from_span_start(self.make_position())),
            '-' => Some(Minus::from_span_start(self.make_position())),
            '*' => Some(Star::from_span_start(self.make_position())),
            '/' => Some(Slash::from_span_start(self.make_position())),
            ' ' | '\n' => None,
            _ => panic!("Unknown token `{}`", char),
        };

        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn simple_test() {
        let input = r"
+ - 
";

        let tokens: Vec<_> = Lexer::run(input.as_bytes()).collect();
        println!("{tokens:#?}")
    }
}
