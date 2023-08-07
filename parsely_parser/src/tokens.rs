use parsely_lexer::tokens::{self, Token};

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
    A,
    And,
    Arguments,
    ArrayTy,
    BoolTy,
    By,
    Called,
    Constant,
    Contains,
    Define,
    Defines,
    Evaluates,
    Executes,
    Executing,
    Function,
    FloatTy,
    IntTy,
    Inputs,
    Of,
    Output,
    Result,
    Starts,
    That,
    Then,
    The,
    Type,
    Value,
    Variable,
    With,

    Adding,
    Subtracting,
    Multiplying,
    Dividing,

    Negating,

    // Punctuation
    Semi,
    Colon,
    Comma,

    Ident,
    Int,
    Float,
    Bool,
    String,
    Char,
}
