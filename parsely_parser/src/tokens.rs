use parsely_lexer::tokens::{self, Token, Group};

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
    // Keyword
    Const,
    Continue,
    Break,
    Else,
    Enum,
    Export,
    External,
    For,
    If,
    Match,
    Nones,
    Persist,
    Return,
    Struct,
    Typedef,
    Typeof,
    Void,
    While,

    // Punctuation
    Semi,
    Colon,
    Comma,
    Pound,

    // Operators
    And,
    AndEq,
    Assign,
    Eq,
    Dot,
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
    Plus,
    PlusEq,
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
}

pub trait ParseGrouped {

}

impl ParseGrouped for Group {

}

