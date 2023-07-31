use std::fmt::Display;

macro_rules! define_tokens {
    ($tok_macro:ident; $vis:vis enum $token_enum:ident { $($struct_name:ident = $st:tt),*, $(,)* }) => {
        $(
            #[derive(Debug, Clone, PartialEq, Eq)]
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

            impl std::fmt::Display for $struct_name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    write!(f, "{}", stringify!($st))
                }
            }
        )*

        #[derive(Debug, Clone, PartialEq)]
        $vis enum $token_enum {
            Ident(Ident),
            Int(Int),
            Float(Float),
            Bool(Bool),
            String(String),
            Char(Char),
            Group(Group),
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
                    $token_enum::Group(_) => 0,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub value: std::string::String,
    pub(crate) span: crate::Span,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Int {
    pub value: u64,
    pub(crate) span: crate::Span,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Float {
    pub value: f64,
    pub(crate) span: crate::Span,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bool {
    pub value: bool,
    pub(crate) span: crate::Span,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct String {
    pub value: std::string::String,
    pub(crate) span: crate::Span,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Char {
    pub value: char,
    pub(crate) span: crate::Span,
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

define_tokens! {
    Tok;
    pub enum Token {
        // Keyword
        Const = const,
        Continue = continue,
        Break = break,
        Else = else,
        Enum = enum,
        Export = export,
        External = external,
        For = for,
        If = if,
        Match = match,
        Nones = none,
        Opaque = opaque,
        Packed = packed,
        Persist = persist,
        Return = return,
        Struct = struct,
        Typedef = typedef,
        Typeof = typeof,
        Void = void,
        While = while,

        // Punctuation
        Semi = ;,
        Colon = :,
        Comma = ,,
        Pound = #,

        // Operators
        And = &,
        AndEq = &=,
        Assign = =,
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

pub use Tok;

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
