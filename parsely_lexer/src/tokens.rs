use std::fmt::Display;

macro_rules! define_tokens {
    ($tok_macro:ident; $vis:vis enum $token_enum:ident { $($struct_name:ident = $st:tt),*, $(,)* }) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            $vis struct $struct_name(pub $crate::Span);

            impl $struct_name {
                pub const EMPTY: $struct_name = $struct_name($crate::Span::EMPTY);
                pub const NAME: &str = stringify!($st);

                pub fn from_span_start(start: $crate::Position) -> $token_enum {
                    $token_enum::$struct_name($struct_name($crate::Span {
                        end: $crate::Position {
                            line: start.line,
                            column: start.column + $struct_name::len(),
                        },
                        start,
                    }))
                }

                pub fn len() -> usize {
                    stringify!($st).len()
                }

                pub fn as_str() -> &'static str {
                    stringify!($st)
                }
            }

            impl AsRef<str> for $struct_name {
                fn as_ref(&self) -> &str {
                    stringify!($st)
                }
            }

            impl Into<$token_enum> for $struct_name {
                fn into(self)  -> $token_enum {
                    $token_enum::$struct_name(self)
                }
            }

            impl std::fmt::Display for $struct_name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    write!(f, "{}", stringify!($st))
                }
            }

            impl $crate::AsSpan for $struct_name {
                fn as_span(&self) -> $crate::Span {
                    self.0
                }
            }

            impl PartialEq<Token> for $struct_name {
                fn eq(&self, other: &Token) -> bool {
                    match other {
                        Token::$struct_name{ .. } => true,
                        _ => false,
                    }
                }
            }
        )*

            #[derive(Debug, Clone, PartialEq, Eq)]
            $vis struct Shebang(pub $crate::Span);

            impl Shebang {
                pub const EMPTY: Self = Self($crate::Span::EMPTY);
                pub const NAME: &str = "#!";

                pub fn from_span_start(start: $crate::Position) -> $token_enum {
                    $token_enum::Shebang(Self($crate::Span {
                        end: $crate::Position {
                            line: start.line,
                            column: start.column + Self::len(),
                        },
                        start,
                    }))
                }

                pub fn len() -> usize {
                    2
                }

                pub fn as_str() -> &'static str {
                    "#!"
                }
            }

            impl std::fmt::Display for Shebang {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    write!(f, "#!")
                }
            }

            impl $crate::AsSpan for Shebang {
                fn as_span(&self) -> $crate::Span {
                    self.0
                }
            }

        #[derive(Debug, Clone, PartialEq)]
        $vis enum $token_enum {
            Ident(Ident),
            Int(Int),
            Float(Float),
            Bool(Bool),
            String(String),
            Char(Char),
            Group(Group),
            Template(Template),
            Shebang(Shebang),
            Newline(Newline),
            DocComment(DocComment),
            Eof($crate::Span),
           $(
                $struct_name($struct_name)
           ),*
        }

        impl $token_enum {
            pub fn len(&self) -> usize {
                match self {
                    $token_enum::Ident(i) => i.span.len(),
                    $token_enum::Int(i) => i.span.len(),
                    $token_enum::Float(i) => i.span.len(),
                    $token_enum::Bool(i) => i.span.len(),
                    $token_enum::String(i) => i.span.len(),
                    $token_enum::Char(i) => i.span.len(),
                    $token_enum::Template(i) => i.span.len(),
                    $token_enum::Newline(i) => i.span.len(),
                    $token_enum::DocComment(i) => i.span.len(),
                    $token_enum::Group(_) => 0,
                    $token_enum::Eof(_) => 0,
                    $token_enum::Shebang(_) => Shebang::len(),
                    $(
                        $token_enum::$struct_name(_) => $struct_name::len()
                    ),*
                }
            }

            pub fn is_empty(&self) -> bool {
                self.len() == 0
            }

            pub fn as_str(&self) -> &'static str {
                match self {
                    $(
                        $token_enum::$struct_name(_) => $struct_name::as_str()
                    ),*,
                    _ => panic!("Unable to get static str on token! (maybe use .to_string() instead)")
                }
            }
        }

        impl $crate::AsSpan for $token_enum {
            fn as_span(&self) -> $crate::Span {
                match self {
                    $token_enum::Ident(i) => i.as_span(),
                    $token_enum::Int(i) => i.as_span(),
                    $token_enum::Float(i) => i.as_span(),
                    $token_enum::Bool(i) => i.as_span(),
                    $token_enum::String(i) => i.as_span(),
                    $token_enum::Char(i) => i.as_span(),
                    $token_enum::Group(i) => i.as_span(),
                    $token_enum::Template(i) => i.as_span(),
                    $token_enum::Shebang(i) => i.as_span(),
                    $token_enum::Newline(i) => i.as_span(),
                    $token_enum::DocComment(i) => i.as_span(),
                    $token_enum::Eof(i) => *i,
                    $(
                        $token_enum::$struct_name(i) => i.as_span(),
                    )*
                }
            }
        }

        impl std::fmt::Display for $token_enum {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                match self {
                    $token_enum::Ident(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Int(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Float(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Bool(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::String(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Char(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Group(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Template(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Shebang(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::DocComment(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Newline(_) => write!(f, "NL"),
                    $token_enum::Eof(_) => write!(f, "EOF"),
                    $(
                        $token_enum::$struct_name(tok) => std::fmt::Display::fmt(tok, f)
                    ),*
                }
            }
        }

        #[macro_export]
        macro_rules! $tok_macro {
            $(
                [$st] => {$crate::tokens::$struct_name};
            )*
            $(
                [enum $st] => {$crate::tokens::$token_enum::$struct_name($crate::tokens::$struct_name(_))};
            )*
            $(
                [enum $st as $name:ident] => {$crate::tokens::$token_enum::$struct_name($name @ $crate::tokens::$struct_name(_))};
            )*
        }
    };
}

// impl PartialEq<Token> for Comma {
//     fn eq(&self, other: &Token) -> bool {
//         match other {
//             Token::Comma(_) => true,
//             _ => false,
//         }
//     }
// }

// impl PartialEq<Token> for Semi {
//     fn eq(&self, other: &Token) -> bool {
//         match other {
//             Token::Semi(_) => true,
//             _ => false,
//         }
//     }
// }

// impl PartialEq<Token> for Or {
//     fn eq(&self, other: &Token) -> bool {
//         match other {
//             Token::Or(_) => true,
//             _ => false,
//         }
//     }
// }

// impl PartialEq<Token> for Dot {
//     fn eq(&self, other: &Token) -> bool {
//         match other {
//             Token::Dot(_) => true,
//             _ => false,
//         }
//     }
// }

// impl PartialEq<Token> for Star {
//     fn eq(&self, other: &Token) -> bool {
//         match other {
//             Token::Star(_) => true,
//             _ => false,
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Newline {
    pub span: crate::Span,
}

impl Newline {
    pub const NAME: &str = "Newline";
}

impl Display for Newline {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Newline::NAME)
    }
}

impl crate::AsSpan for Newline {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: std::string::String,
    pub span: crate::Span,
}

impl Ident {
    pub const NAME: &str = "Identifier";

    pub fn from_span_start(raw: &[char], start: crate::Position) -> Token {
        Token::Ident(Ident {
            value: raw.iter().collect(),
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl crate::AsSpan for Ident {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

impl Into<Token> for Ident {
    fn into(self) -> Token {
        Token::Ident(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Int {
    pub value: u64,
    pub span: crate::Span,
}

impl Int {
    pub const NAME: &str = "Integer";

    pub fn from_span_start(raw: &[char], start: crate::Position) -> Token {
        Token::Int(Int {
            value: raw
                .iter()
                .collect::<std::string::String>()
                .parse()
                .expect("Unexpected value in int!"),
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl crate::AsSpan for Int {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

impl Into<Token> for Int {
    fn into(self) -> Token {
        Token::Int(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Float {
    pub value: f64,
    pub span: crate::Span,
}

impl Float {
    pub const NAME: &str = "Float";

    pub fn from_span_start(raw: &[char], start: crate::Position) -> Token {
        Token::Float(Float {
            value: raw
                .iter()
                .collect::<std::string::String>()
                .parse()
                .expect("Unexpected value in float!"),
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl crate::AsSpan for Float {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

impl Into<Token> for Float {
    fn into(self) -> Token {
        Token::Float(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bool {
    pub value: bool,
    pub span: crate::Span,
}

impl Bool {
    pub const NAME: &str = "Boolean";

    pub fn from_value(value: bool, raw: &[char], start: crate::Position) -> Token {
        Token::Bool(Bool {
            value,
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }

    pub fn from_span_start(raw: &str, start: crate::Position) -> Token {
        Token::Bool(Bool {
            value: raw.parse().expect("Unexpected value in bool!"),
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl crate::AsSpan for Bool {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

impl Into<Token> for Bool {
    fn into(self) -> Token {
        Token::Bool(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct String {
    pub value: std::string::String,
    pub span: crate::Span,
}

impl String {
    pub const NAME: &str = "String";

    pub fn from_span_start(raw: &str, start: crate::Position) -> Token {
        Token::String(String {
            value: raw[1..raw.len() - 1].to_string(),
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }
}

impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

impl crate::AsSpan for String {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

impl Into<Token> for String {
    fn into(self) -> Token {
        Token::String(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Char {
    pub value: char,
    pub span: crate::Span,
}

impl Char {
    pub const NAME: &str = "Char";

    pub fn from_span_start(raw: &str, start: crate::Position) -> Token {
        Token::Char(Char {
            value: raw[1..raw.len() - 1].to_string().chars().next().unwrap(),
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }
}

impl Display for Char {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.value)
    }
}

impl crate::AsSpan for Char {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

impl Into<Token> for Char {
    fn into(self) -> Token {
        Token::Char(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Template {
    pub value: std::string::String,
    pub span: crate::Span,
}

impl Template {
    pub const NAME: &str = "Template";

    pub fn from_span_start(raw: &str, start: crate::Position) -> Token {
        Token::Template(Template {
            value: raw[2..raw.len() - 1].to_string(),
            span: crate::Span {
                end: crate::Position {
                    line: start.line,
                    column: start.column + raw.len(),
                },
                start,
            },
        })
    }
}

impl Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${{{}}}'", self.value)
    }
}

impl crate::AsSpan for Template {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DocComment {
    pub span: Span,
    pub value_span: Span,
    pub value: std::string::String,
}

impl Display for DocComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "/// {}'", self.value)
    }
}

impl crate::AsSpan for DocComment {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

define_tokens! {
    Tok;
    pub enum Token {
        // Keyword
        Let = let,
        Mut = mut,
        Type = type,
        Module = module,
        If = if,
        Then = then,
        Else = else,
        Loop = loop,
        Do = do,
        Of = of,
        In = in,
        True = true,
        False = false,
        Nones = none,
        Match = match,
        With = with,
        Export = export,
        Import = import,
        Const = const,
        Inline = inline,
        Internal = internal,
        Persist = persist,
        Return = return,
        Break = break,
        Continue = continue,

        // Punctuation
        Semi = ;,
        Question = ?,
        Colon = :,
        Comma = ,,
        Pound = #,
        Underscore = _,

        // Operators
        And = &,
        AndEq = &=,
        Assign = =,
        Arrow = =>,
        Eq = ==,
        Dot = .,
        Gt = >,
        GtEq = >=,
        LeftShift = <<,
        LeftShiftEq = <<=,
        LogicalAnd = &&,
        LogicalOr = ||,
        Lt = <,
        LtEq = <=,
        Minus = -,
        MinusEq = -=,
        Not = !,
        NotEq = !=,
        Or = |,
        OrEq = |=,
        Plus = +,
        PlusEq = +=,
        Rem = %,
        RemEq = %=,
        Range = ..,
        RangeEq = ..=,
        RightShift = >>,
        RightShiftEq = >>=,
        Slash = /,
        SlashEq = /=,
        Star = *,
        StarEq = *=,
        Xor = ^,
        XorEq = ^=,
    }
}

impl Token {
    pub fn matches(&self, other: &Token) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}

pub use Tok;

use crate::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Group {
    pub open: crate::Span,
    pub close: crate::Span,
    pub bracket: GroupBracket,
    pub tokens: Vec<Token>,
}

impl Group {
    pub fn parens(self) -> Paren {
        Paren {
            span: self.open.join(self.close),
        }
    }

    pub fn brackets(self) -> Bracket {
        Bracket {
            span: self.open.join(self.close),
        }
    }

    pub fn brace(self) -> Brace {
        Brace {
            span: self.open.join(self.close),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GroupBracket {
    Paren,
    Brace,
    Bracket,
}

impl Display for Group {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.bracket {
            GroupBracket::Paren => write!(f, "(...)"),
            GroupBracket::Brace => write!(f, "{{...}}"),
            GroupBracket::Bracket => write!(f, "[...]"),
        }
    }
}

impl crate::AsSpan for Group {
    fn as_span(&self) -> crate::Span {
        self.open.join(self.close)
    }
}

#[derive(Debug, Clone)]
pub struct Paren {
    pub span: crate::Span,
}

#[derive(Debug, Clone)]
pub struct Brace {
    pub span: crate::Span,
}

#[derive(Debug, Clone)]
pub struct Bracket {
    pub span: crate::Span,
}
