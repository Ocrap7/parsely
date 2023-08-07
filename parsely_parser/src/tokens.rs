use parsely_lexer::tokens::{self, Token, Keyword, KeywordMod, Plural, ThirdPerson, PastParticiple, Gerund};

use crate::{Parse, ParseError, Result};

macro_rules! impl_token_parse {
    ($($struct_name:tt),* $(,)*) => {
        $(
            impl_token_parse! {@parse $struct_name}
        )*
    };
    (@parse [$struct_name:ident]) => {
        impl Parse for tokens::$struct_name {
            fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                match stream.peek()? {
                    Token::Keyword(Keyword::$struct_name(a), _) => Ok(stream.next_ref(a)),
                    tok => Err(ParseError::UnexpectedToken {
                        found: tok.clone(),
                        expected: tokens::$struct_name::NAME.to_string(),
                    }),
                }
            }
        }
    };
    (@parse [:$struct_name:ident]) => {
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
    };
    (@parse [$struct_name:ident :noun]) => {
        impl Parse for tokens::$struct_name<()> {
            fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                match stream.peek()? {
                    Token::Keyword(Keyword::$struct_name(a), _) => Ok(stream.next_ref(a)),
                    tok => Err(ParseError::UnexpectedToken {
                        found: tok.clone(),
                        expected: tokens::$struct_name::NAME.to_string(),
                    }),
                }
            }
        }

        impl Parse for tokens::$struct_name<Plural> {
            fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                match stream.peek()? {
                    Token::Keyword(Keyword::$struct_name(a), KeywordMod::PLURAL) => Ok(stream.next_ref(a)),
                    tok => Err(ParseError::UnexpectedToken {
                        found: tok.clone(),
                        expected: tokens::$struct_name::NAME.to_string(),
                    }),
                }
            }
        }
    };
    (@parse [$struct_name:ident :verb]) => {
        impl Parse for tokens::$struct_name<()> {
            fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                match stream.peek()? {
                    Token::Keyword(Keyword::$struct_name(a), _) => Ok(stream.next_ref(a)),
                    tok => Err(ParseError::UnexpectedToken {
                        found: tok.clone(),
                        expected: tokens::$struct_name::NAME.to_string(),
                    }),
                }
            }
        }

        impl Parse for tokens::$struct_name<ThirdPerson> {
            fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                match stream.peek()? {
                    Token::Keyword(Keyword::$struct_name(a), KeywordMod::THIRD_PERSON) => Ok(stream.next_ref(a)),
                    tok => Err(ParseError::UnexpectedToken {
                        found: tok.clone(),
                        expected: tokens::$struct_name::NAME.to_string(),
                    }),
                }
            }
        }

        impl Parse for tokens::$struct_name<PastParticiple> {
            fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                match stream.peek()? {
                    Token::Keyword(Keyword::$struct_name(a), KeywordMod::PAST_PARTICIPLE) => Ok(stream.next_ref(a)),
                    tok => Err(ParseError::UnexpectedToken {
                        found: tok.clone(),
                        expected: tokens::$struct_name::NAME.to_string(),
                    }),
                }
            }
        }

        impl Parse for tokens::$struct_name<Gerund> {
            fn parse(stream: &'_ crate::ParseStream<'_>) -> Result<Self> {
                match stream.peek()? {
                    Token::Keyword(Keyword::$struct_name(a), KeywordMod::GERUND) => Ok(stream.next_ref(a)),
                    tok => Err(ParseError::UnexpectedToken {
                        found: tok.clone(),
                        expected: tokens::$struct_name::NAME.to_string(),
                    }),
                }
            }
        }
    };

}

impl_token_parse! {
    // Keyword
    [A],
    [And],
    [Argument:noun],
    [ArrayTy],
    [BoolTy],
    [By],
    [Call:verb],
    [Constant],
    [Contain:verb],
    [Define:verb],
    [Evaluate:verb],
    [Execute:verb],
    [Function],
    [FloatTy],
    [IntTy],
    [Input:noun],
    [Of],
    [Output],
    [Result],
    [Start:verb],
    [That],
    [Then],
    [The],
    [Type],
    [Value],
    [Variable],
    [With],

    [Add:verb],
    [Subtract:verb],
    [Multiply:verb],
    [Divide:verb],

    [Negate:verb],

    // Punctuation
    [:Semi],
    [:Colon],
    [:Comma],

    [:Ident],
    [:Int],
    [:Float],
    [:Bool],
    [:String],
    [:Char],
}
