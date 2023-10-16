use parsely_analysis::{EnumResolves, Type};
use parsely_lexer::{AsSpan, Span};
use parsely_parser::{
    expression::{Expression, Literal},
    item::{Argument, Element, ElementBody, Item},
    program::Program,
};
use tower_lsp::lsp_types::{
    CompletionItem, SemanticToken, SemanticTokenModifier, SemanticTokenType,
};

macro_rules! define_types {
    ($($ty:ident),*) => {
        pub const STOKEN_TYPES: &[SemanticTokenType] = &[
            $(SemanticTokenType::$ty),*
        ];

        define_types!{@(0; $($ty),*,)}
    };
    (@($idx:expr; $ty:ident, $($rest:tt)*) $($arms:tt)*) => {
        define_types!{
            @(1 + $idx; $($rest)*)
            $($arms)*
            [$ty] => { $idx };
        }
    };
    (@($idx:expr; $(,)?) $($arms:tt)*) => {
        macro_rules! semantic_type {
            $($arms)*
        }
    };
}

macro_rules! define_mods {
    ($($ty:ident : $ex:expr),*) => {
        pub const STOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
            $($ex),*
        ];

        define_mods!{@(1; $($ty),*,)}
    };
    (@($idx:expr; $ty:ident, $($rest:tt)*) $($arms:tt)*) => {
        define_mods!{
            @(1 + $idx; $($rest)*)
            $($arms)*
            [$ty] => { $idx };
        }
    };
    (@($idx:expr; $(,)?) $($arms:tt)*) => {
        macro_rules! semantic_mod {
            $($arms)*
        }
    };
}

define_types! {
    KEYWORD,
    TYPE,
    VARIABLE,
    NAMESPACE,
    CLASS,
    ENUM,
    INTERFACE,
    STRUCT,
    TYPE_PARAMETER,
    PARAMETER,
    PROPERTY,
    ENUM_MEMBER,
    EVENT,
    FUNCTION,
    METHOD,
    MACRO,
    MODIFIER,
    COMMENT,
    STRING,
    NUMBER,
    REGEXP,
    OPERATOR
}

define_mods! {
    CONST : SemanticTokenModifier::new("const"),
    READONLY : SemanticTokenModifier::new("readonly")
}

#[derive(Default)]
pub struct SemanticTokenBuilder {
    tokens: Vec<SemanticToken>,
    last_line: u32,
    last_pos: u32,
}

impl SemanticTokenBuilder {
    pub fn push(&mut self, line: u32, position: u32, length: u32, token: u32, modifier: u32) {
        if self.last_line == line {
            let delta_pos = position - self.last_pos;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line: 0,
                delta_start: delta_pos,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        } else {
            let delta_line = line - self.last_line;
            self.last_line = line;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line,
                delta_start: position,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        }
    }

    pub fn push_token<T: AsSpan>(
        &mut self,
        token: &T,
        semantic_type_index: u32,
        semantic_modifier_index: u32,
    ) {
        let span = token.as_span();

        self.push(
            span.start.line as u32,
            span.start.column as u32,
            span.len() as u32,
            semantic_type_index,
            semantic_modifier_index,
        )
    }
}

macro_rules! brr {
            ($($t:ident),*) => {
                [$(
                    (CompletionItemKind::$t, stringify!($t))
                ),*]
            };
        }
pub struct SemanticGenerator<'a> {
    resolves: &'a EnumResolves<'static>,
    tokens: SemanticTokenBuilder,
    completes: Vec<(CompletionItem, Span)>,
}

impl<'a> SemanticGenerator<'a> {
    pub fn new(resolves: &'a EnumResolves<'static>) -> Self {
        Self {
            resolves,
            tokens: SemanticTokenBuilder::default(),
            completes: Vec::new(),
        }
    }

    pub fn run<'b>(&mut self, program: &'b Program) {
        for item in program.items.iter() {
            self.run_item(item);
        }
    }

    pub fn finish(self) -> (Vec<SemanticToken>, Vec<(CompletionItem, Span)>) {
        (self.tokens.tokens, self.completes)
    }
}

impl SemanticGenerator<'_> {
    pub fn run_item<'b>(&mut self, item: &'b Item) {
        match item {
            Item::Element(elem) => {
                if let Some(args) = &elem.args {
                    for arg in args.0.value.iter() {
                        self.run_argument(elem, arg)
                    }
                }

                match &elem.body {
                    ElementBody::Children(children) => {
                        for child in children.value.iter() {
                            self.run_item(child)
                        }
                    }
                    ElementBody::Child(_) => {}
                }
            }
            Item::Expression(expr) => {}
            Item::Inputs(_) => {}
        }
    }

    pub fn run_argument<'b>(&mut self, element: &'b Element, arg: &'b Argument) {
        let Some(attrs) = self.resolves.get(&element.tag.value) else {
            return;
        };

        let Some(ty) = attrs.get(&arg.key.value) else {
            return;
        };

        let Some((_, expr)) = &arg.value else {
            return;
        };

        let ty = ty.clone();
        let expr_ty = self.run_expression(&expr);

        if expr_ty.matches(&ty) {
            match ty {
                Type::Enum(_) => {
                    let span = match expr.as_ref() {
                        Expression::Enum(en) => en.ident.as_span(),
                        _ => panic!(),
                    };

                    self.tokens
                        .push_token(&span, semantic_type![ENUM_MEMBER], 0);
                }
                _ => (),
            }
        }
    }

    pub fn run_expression<'b>(&mut self, expr: &'b Expression) -> Type<'b> {
        match expr {
            Expression::ArrayInit(_) => Type::Array,
            Expression::Enum(e) => Type::EnumMember(e.ident.value.as_str().into()),
            Expression::Literal(lit) => match lit {
                Literal::Int(_) | Literal::Float(_) => Type::Number,
                Literal::String(_) => Type::String,
            },
            Expression::Template(_) => Type::String,
            Expression::Ident(_) => Type::Empty,
        }
    }
}
