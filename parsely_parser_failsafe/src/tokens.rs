use parsely_lexer::tokens::{self, Group, Token};

use crate::{dummy::Dummy, Parse, Result};
use parsely_diagnostics::Diagnostic;

macro_rules! impl_token_parse {
    ($($struct_name:ident),*, $(,)*) => {
        $(
            impl Parse for tokens::$struct_name {
                fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
                    match stream.peek()? {
                        Token::$struct_name(a) => Ok(stream.next_ref(a)),
                        // _ => Err(tokens::$struct_name::new_dummy(stream.current_position())),
                        tok => Err(Diagnostic::UnexpectedToken {
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
    Let,
    Mut,
    Type,
    Module,
    If,
    Then,
    Else,
    Loop,
    Do,
    Of,
    In,
    True,
    False,
    Nones,
    Match,
    With,
    Export,
    Import,
    Const,
    Inline,
    Internal,
    Persist,
    Return,
    Break,
    Continue,

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
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        stream.next()
    }
}

pub trait ParseGrouped {}

impl ParseGrouped for Group {}
