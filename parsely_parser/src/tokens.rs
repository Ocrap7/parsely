use parsely_lexer::tokens::{self, Group, Token};

use crate::{Parse, ParseError, Result};

macro_rules! impl_token_parse {
    ($($struct_name:ident),*, $(,)*) => {
        $(
            impl Parse for tokens::$struct_name {
                fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                    match stream.peek()? {
                        Token::$struct_name(a) => Ok(stream.next_ref(a)),
                        tok => Err(ParseError::UnexpectedToken {
                            found: tok.clone(),
                            expected: tokens::$struct_name::NAME.to_string(),
                        }),
                    }
                }
            }
        )*
    };
}

impl_token_parse! {
    Parameters,
    Template,

    // Punctuation
    Semi,
    Question,
    Colon,
    Comma,
    Pound,
    Shebang,
    Underscore,
    And,
    AndEq,
    Assign,
    Arrow,
    Eq,
    Dot,
    DotStar,
    Gt,
    GtEq,
    LeftShift,
    LeftShiftEq,
    LogicalAnd,
    LogicalOr,
    Lt,
    LtEq,
    Minus,
    MinusEq,
    Not,
    NotEq,
    Or,
    OrEq,
    Plus,
    PlusEq,
    Range,
    RangeEq,
    Rem,
    RemEq,
    RightShift,
    RightShiftEq,
    Slash,
    SlashEq,
    Star,
    StarEq,
    Xor,
    XorEq,


    Ident,
    Int,
    Float,
    Bool,
    String,
    Char,
    Newline,
}

impl Parse for Token {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
        Ok(stream.next())
    }
}

pub trait ParseGrouped {}

impl ParseGrouped for Group {}
