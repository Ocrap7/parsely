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
    Input,
    Template,

    // Punctuation
    Semi,
    Question,
    Colon,
    Comma,
    Pound,

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

