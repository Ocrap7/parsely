use std::fmt::Display;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Range(pub std::ops::Range<usize>);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Plural;
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ThirdPerson;
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PastParticiple;
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Gerund;

macro_rules! define_tokens {
    ($tok_macro:ident; $vis:vis enum $token_enum:ident, $keyword_enum:ident => $keyword_mod:ident { $($struct_name:tt = $body:tt),* $(,)?; $($struct_name_n:ident = $body_n:tt),* $(,)? }) => {
        $(
            define_tokens! { @arm $vis $token_enum $keyword_enum $keyword_mod $struct_name = $body }
        )*
        $(
            define_tokens! { @arm $vis $token_enum [$struct_name_n] = $body_n }
        )*
        define_tokens! { @kw_enum $vis $keyword_enum $($struct_name = $body),* }
        pub const SSSS: &str = stringify!(
            $(
              $struct_name_n
            ),*
        );

        bitflags::bitflags! {
            #[derive(Debug, Clone, PartialEq, Eq)]
            $vis struct $keyword_mod: u64 {
                const EMPTY = 0;
                const NOUN = 0x1;
                const PLURAL = Self::NOUN.bits() | 0x2;

                const VERB = 0x4;
                const THIRD_PERSON = Self::VERB.bits() | 0x8;
                const PAST_PARTICIPLE = Self::VERB.bits() | 0x10;
                const GERUND = Self::VERB.bits() | 0x20;
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
            $keyword_enum($keyword_enum, $keyword_mod),
            Eof($crate::Span),
            $($struct_name_n($struct_name_n)),*,
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
                    $token_enum::Eof(_) => 0,
                    $token_enum::$keyword_enum(k, _) => k.len(),
                    $(
                      $token_enum::$struct_name_n(_) => $struct_name_n::len()
                    ),*
                }
            }

            pub fn is_empty(&self) -> bool {
                self.len() == 0
            }

            pub fn as_str(&self) -> &'static str {
                match self {
                    $token_enum::$keyword_enum(k, _) => k.as_str(),
                    _ => panic!("Unable to get static str on token! (maybe use .to_string() instead)")
                }
            }
        }

        impl std::fmt::Display for $token_enum {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                match self {
                    $token_enum::Ident(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Int(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Float(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Bool(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::String(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Char(i) => std::fmt::Display::fmt(i, f),
                    $token_enum::Group(i) => std::fmt::Display::fmt(i, f),
                    $(
                        $token_enum::$struct_name_n(i) => std::fmt::Display::fmt(i, f),
                    )*
                    $token_enum::$keyword_enum(k, _) => std::fmt::Display::fmt(k, f),
                    $token_enum::Eof(_) => write!(f, "eof"),
                }
            }
        }

        impl AsSpan for $token_enum {
            fn as_span(&self) -> $crate::Span {
                match self {
                    $token_enum::Ident(i) => i.as_span(),
                    $token_enum::Int(i) => i.as_span(),
                    $token_enum::Float(i) => i.as_span(),
                    $token_enum::Bool(i) => i.as_span(),
                    $token_enum::String(i) => i.as_span(),
                    $token_enum::Char(i) => i.as_span(),
                    $token_enum::Group(i) => i.as_span(),
                    $token_enum::$keyword_enum(k, _) => k.as_span(),
                    $(
                        $token_enum::$struct_name_n(i) => i.as_span(),
                    )*
                    $token_enum::Eof(i) => *i,
                }
            }
        }

        define_tokens! {@impl_match_macro $tok_macro $token_enum $keyword_enum $keyword_mod $($struct_name = $body),*, $([$struct_name_n] = $body_n),*}
    };
    (@arm $vis:vis $token_enum:ident [$struct_name:ident] = $st:tt) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $struct_name(pub $crate::Span);

        impl $struct_name {
            pub const EMPTY: $struct_name = $struct_name($crate::Span::EMPTY);
            pub const NAME: &str = stringify!($st);

            pub fn from_span_start(start: $crate::Position, len: usize) -> $token_enum {
                $token_enum::$struct_name(
                    $crate::tokens::$struct_name(
                        $crate::Span {
                            end: $crate::Position {
                                line: start.line,
                                column: start.column + len,
                            },
                            start,
                        }
                    )
                )
            }

            pub fn len() -> usize {
                stringify!($st).len()
            }

            pub fn as_str() -> &'static str {
                stringify!($st)
            }
        }

        impl std::fmt::Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                write!(f, "{}", stringify!($st))
            }
        }

        impl $crate::AsSpan for $struct_name {
            fn as_span(&self) -> $crate::Span {
                self.0
            }
        }
    };
    (@arm $vis:vis $token_enum:ident $keyword_enum:ident $keyword_mod:ident [$struct_name:ident] = $st:tt) => {
            #[derive(Debug, Clone, PartialEq, Eq)]
            $vis struct $struct_name(pub $crate::Span);

            impl $struct_name {
                pub const EMPTY: $struct_name = $struct_name($crate::Span::EMPTY);
                pub const NAME: &str = stringify!($st);

                pub fn from_span_start(start: $crate::Position, len: usize, mods: $keyword_mod) -> $token_enum {
                    $token_enum::$keyword_enum(
                        $keyword_enum::$struct_name(
                            $crate::tokens::$struct_name($crate::Span {
                                end: $crate::Position {
                                    line: start.line,
                                    column: start.column + len,
                                },
                                start,
                            })
                        ),
                        mods
                    )
                }

                pub fn len() -> usize {
                    stringify!($st).len()
                }

                pub fn as_str() -> &'static str {
                    stringify!($st)
                }
            }

            impl std::fmt::Display for $struct_name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                    write!(f, "{}", stringify!($st))
                }
            }

            impl $crate::AsSpan for $struct_name {
                fn as_span(&self) -> $crate::Span {
                    self.0
                }
            }
    };
    (@arm $vis:vis $token_enum:ident $keyword_enum:ident $keyword_mod:ident [$struct_name:ident :noun] = $st:tt) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $struct_name<Plural = ()>(pub $crate::Span, pub std::marker::PhantomData<Plural>);

        impl $struct_name<()> {
            pub const EMPTY: $struct_name = $struct_name::<()>($crate::Span::EMPTY, std::marker::PhantomData);
            pub const NAME: &str = stringify!($st);

            pub fn from_span_start(start: $crate::Position, len: usize, mods: $keyword_mod) -> $token_enum {
                $token_enum::$keyword_enum(
                    $keyword_enum::$struct_name(
                        $crate::tokens::$struct_name(
                            $crate::Span {
                                end: $crate::Position {
                                    line: start.line,
                                    column: start.column + len,
                                },
                                start,
                            },
                            std::marker::PhantomData
                        )
                    ),
                    mods
                )
            }

            pub fn len() -> usize {
                stringify!($st).len()
            }

            pub fn as_str() -> &'static str {
                stringify!($st)
            }
        }

        impl <P> std::fmt::Display for $struct_name<P> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                write!(f, "{}", stringify!($st))
            }
        }

        impl <P> $crate::AsSpan for $struct_name<P> {
            fn as_span(&self) -> $crate::Span {
                self.0
            }
        }

        impl From<$struct_name<()>> for $struct_name<$crate::tokens::Plural> {
            fn from(value: $struct_name<()>) -> Self {
                Self(value.0, std::marker::PhantomData)
            }
        }
    };
    (@arm $vis:vis $token_enum:ident $keyword_enum:ident $keyword_mod:ident [$struct_name:ident :verb] = $st:tt) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $struct_name<Form = ()>(pub $crate::Span, pub std::marker::PhantomData<Form>);

        impl $struct_name<()> {
            pub const EMPTY: $struct_name = $struct_name::<()>($crate::Span::EMPTY, std::marker::PhantomData);
            pub const NAME: &str = stringify!($st);

            pub fn from_span_start(start: $crate::Position, len: usize, mods: $keyword_mod) -> $token_enum {
                $token_enum::$keyword_enum(
                    $keyword_enum::$struct_name(
                        $crate::tokens::$struct_name(
                            $crate::Span {
                                end: $crate::Position {
                                    line: start.line,
                                    column: start.column + len,
                                },
                                start,
                            },
                            std::marker::PhantomData
                        )
                    ),
                    mods
                )
            }

            pub fn len() -> usize {
                stringify!($st).len()
            }

            pub fn as_str() -> &'static str {
                stringify!($st)
            }
        }

        impl <P> std::fmt::Display for $struct_name<P> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                write!(f, "{}", stringify!($st))
            }
        }

        impl <P> $crate::AsSpan for $struct_name<P> {
            fn as_span(&self) -> $crate::Span {
                self.0
            }
        }

        impl From<$struct_name<()>> for $struct_name<$crate::tokens::ThirdPerson> {
            fn from(value: $struct_name<()>) -> Self {
                Self(value.0, std::marker::PhantomData)
            }
        }

        impl From<$struct_name<()>> for $struct_name<$crate::tokens::PastParticiple> {
            fn from(value: $struct_name<()>) -> Self {
                Self(value.0, std::marker::PhantomData)
            }
        }

        impl From<$struct_name<()>> for $struct_name<$crate::tokens::Gerund> {
            fn from(value: $struct_name<()>) -> Self {
                Self(value.0, std::marker::PhantomData)
            }
        }
    };
    (@kw_enum $vis:vis $keyword_enum:ident $([$struct_name:ident $(:noun)? $(:verb)?] = $st:tt),* $(,)* ) => {
        #[derive(Debug, Clone, PartialEq)]
        $vis enum $keyword_enum {
           $(
                $struct_name($struct_name)
           ),*
        }

        impl $keyword_enum {
            pub fn len(&self) -> usize {
                match self {
                    $(
                        $keyword_enum::$struct_name(_) => $struct_name::len()
                    ),*
                }
            }

            pub fn is_empty(&self) -> bool {
                self.len() == 0
            }

            pub fn as_str(&self) -> &'static str {
                match self {
                    $(
                        $keyword_enum::$struct_name(_) => $struct_name::as_str()
                    ),*,
                }
            }
        }

        impl Display for $keyword_enum {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        $keyword_enum::$struct_name(tok) => std::fmt::Display::fmt(tok, f)
                    ),*
                }
            }
        }

        impl AsSpan for $keyword_enum {
            fn as_span(&self) -> $crate::Span {
                match self {
                    $(
                        $keyword_enum::$struct_name(tok) => tok.as_span()
                    ),*
                }
            }
        }
    };
    (@impl_match_macro $tok_macro:ident $token_enum:ident $keyword_enum:ident $keyword_mod:ident $([$struct_name:ident $(:noun)? $(:verb)? ] = $st:tt),* $(,)*) => {
        #[macro_export]
        macro_rules! $tok_macro {
            $(
                [$st] => {$crate::tokens::$struct_name};
            )*
            $(
                [$st:plural] => {$crate::tokens::$struct_name::<$crate::tokens::Plural>};
            )*
            $(
                [$st:third_person] => {$crate::tokens::$struct_name::<$crate::tokens::ThirdPerson>};
            )*
            $(
                [$st:past_participle] => {$crate::tokens::$struct_name::<$crate::tokens::PastParticiple>};
            )*
            $(
                [$st:gerund] => {$crate::tokens::$struct_name::<$crate::tokens::Gerund>};
            )*

            $(
                [enum $st] => {$crate::tokens::$token_enum::$struct_name($crate::tokens::$struct_name(..))};
            )*

            $(
                [enum $st:other] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($crate::tokens::$struct_name(..)), _)};
            )*

            // Nouns
            $(
                [enum $st:noun] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::NOUN)};
            )*
            $(
                [enum $st:plural] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::PLURAL)};
            )*

            // Verbs
            $(
                [enum $st:verb] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::VERB)};
            )*
            $(
                [enum $st:third_person] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::THIRD_PERSON)};
            )*
            $(
                [enum $st:past_participle] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::PAST_PARTICIPLE)};
            )*
            $(
                (enum $st:gerund) => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::GERUND)};
            )*

            // /** Bindings **/

            $(
                [enum $name:ident @ $st] => {$crate::tokens::$token_enum::$struct_name($name @ $crate::tokens::$struct_name(..))};
            )*

            // Nouns
            $(
                [enum $name:ident @ $st:noun] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($name @ $crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::NOUN)};
            )*
            $(
                [enum $name:ident @ $st:plural] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($name @ $crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::PLURAL)};
            )*

            // Verbs
            $(
                [enum $name:ident @ $st:verb] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($name @ $crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::VERB)};
            )*
            $(
                [enum $name:ident @ $st:third_person] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($name @ $crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::THIRD_PERSON)};
            )*
            $(
                [enum $name:ident @ $st:past_participle] => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($name @ $crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::PAST_PARTICIPLE)};
            )*
            $(
                (enum $name:ident @ $st:gerund) => {$crate::tokens::$token_enum::$keyword_enum($crate::tokens::$keyword_enum::$struct_name($name @ $crate::tokens::$struct_name(..)), $crate::tokens::$keyword_mod::GERUND)};
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

impl AsSpan for Ident {
    fn as_span(&self) -> crate::Span {
        self.span
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

impl AsSpan for Int {
    fn as_span(&self) -> crate::Span {
        self.span
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

impl AsSpan for Float {
    fn as_span(&self) -> crate::Span {
        self.span
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

impl AsSpan for Bool {
    fn as_span(&self) -> crate::Span {
        self.span
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

impl AsSpan for String {
    fn as_span(&self) -> crate::Span {
        self.span
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

impl AsSpan for Char {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

define_tokens! {
    Tok;
    pub enum Token, Keyword => KeywordMod {
        // Keyword
        [A] = a,
        [And] = and,
        [Argument:noun] = argument,
        [ArrayTy] = array,
        [BoolTy] = boolean,
        [By] = by,
        [Call:verb] = call,
        [Constant:noun] = constant,
        [Contain:verb] = contain,
        [Define:verb] = define,
        [Evaluate:verb] = evaluate,
        [Execute:verb] = execute,
        [Function:noun] = function,
        [FloatTy] = float,
        [Input:noun] = input,
        [IntTy] = integer,
        [Of] = of,
        [Output:noun] = output,
        [Result:noun] = result,
        [Start:verb] = start,
        [That] = that,
        [Then] = then,
        [The] = the,
        [Type:noun] = type,
        [Value:noun] = value,
        [Variable:noun] = variable,
        [With] = with,

        // Operator
        [Add:verb] = add,
        [Subtract:verb] = subtract,
        [Multiply:verb] = multiply,
        [Divide:verb] = divide,

        [Negate:verb] = negate
        ;

        // Punctuation
        Semi = ;,
        Comma = ,,
        Colon = :,
    }
}

pub use Tok;

use crate::AsSpan;

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

impl AsSpan for Group {
    fn as_span(&self) -> crate::Span {
        self.open.join(self.close)
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

impl AsSpan for Paren {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Brace {
    pub span: crate::Span,
}

impl AsSpan for Brace {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct Bracket {
    pub span: crate::Span,
}

impl AsSpan for Bracket {
    fn as_span(&self) -> crate::Span {
        self.span
    }
}
