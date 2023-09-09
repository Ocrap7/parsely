use parsely_lexer::{
    tokens::{self, Bool, Char, Float, Ident, Int, Newline},
    Position, Span,
};

macro_rules! impl_dummy {
    ($($struct_name:ident),*, $(,)*) => {
        $(
            impl Dummy for tokens::$struct_name {
                fn new_dummy(position: Position) -> Self {
                    Self(parsely_lexer::Span::new_dummy(position))
                }
            }
        )*
    };
}

pub trait Dummy {
    fn new_dummy(position: Position) -> Self;
}

impl_dummy! {
    Let,

    Semi,
    Question,
    Colon,
    Comma,
    Pound,
    Shebang,
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
    OrEq,
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
}

impl Dummy for Ident {
    fn new_dummy(position: Position) -> Self {
        Self {
            value: String::new(),
            span: Span::new_dummy(position),
        }
    }
}

impl Dummy for Int {
    fn new_dummy(position: Position) -> Self {
        Self {
            value: 0,
            span: Span::new_dummy(position),
        }
    }
}

impl Dummy for Float {
    fn new_dummy(position: Position) -> Self {
        Self {
            value: 0.0,
            span: Span::new_dummy(position),
        }
    }
}

impl Dummy for Bool {
    fn new_dummy(position: Position) -> Self {
        Self {
            value: false,
            span: Span::new_dummy(position),
        }
    }
}

impl Dummy for tokens::String {
    fn new_dummy(position: Position) -> Self {
        Self {
            value: "".into(),
            span: Span::new_dummy(position),
        }
    }
}

impl Dummy for Char {
    fn new_dummy(position: Position) -> Self {
        Self {
            value: '\0',
            span: Span::new_dummy(position),
        }
    }
}

impl Dummy for Newline {
    fn new_dummy(position: Position) -> Self {
        Self {
            span: Span::new_dummy(position),
        }
    }
}
