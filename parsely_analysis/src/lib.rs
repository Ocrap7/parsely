use std::{borrow::Cow, collections::HashMap, fmt::Display};

use parsely_diagnostics::{Diagnostic, DiagnosticLevel};
use parsely_lexer::AsSpan;
use parsely_parser::{
    expression::{Expression, Literal},
    item::{Argument, Element, ElementBody, Item},
    program::Program,
};

/// tag -> attribute -> enum value
pub type EnumResolves<'a> = HashMap<String, HashMap<String, Type<'a>>>;

#[derive(Clone)]
pub enum Type<'a> {
    Empty,
    EnumMember(Cow<'a, str>),
    Enum(Vec<String>),
    String,
    Array,
    Number,
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Empty => write!(f, "empty"),
            Type::EnumMember(val) => write!(f, "{val}"),
            Type::Enum(vars) => write!(f, "{vars:?}"),
            Type::String => write!(f, "string"),
            Type::Array => write!(f, "array"),
            Type::Number => write!(f, "number"),
        }
    }
}

impl Type<'_> {
    /// `self` should be a type 'fitting' into `other`
    pub fn matches(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::EnumMember(val), Type::Enum(variants)) => variants.iter().any(|s| s == val),
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

pub fn enum_resolves<'a>() -> EnumResolves<'a> {
    let mut tags = HashMap::new();

    {
        let input_attrs = [(
            "type",
            Type::Enum(["text"].into_iter().map(ToString::to_string).collect()),
        )];
        let input_tag =
            HashMap::from_iter(input_attrs.into_iter().map(|(k, v)| (k.to_string(), v)));
        tags.insert("input".to_string(), input_tag);
    }

    {
        let a_attrs = [(
            "href",
            Type::String,
        )];
        let a_tag =
            HashMap::from_iter(a_attrs.into_iter().map(|(k, v)| (k.to_string(), v)));
        tags.insert("a".to_string(), a_tag);
    }

    tags
}

pub struct Analyzer<'a> {
    pub diagnostics: Vec<Diagnostic>,

    resolve: EnumResolves<'a>,
}

impl<'a> Analyzer<'a> {
    pub fn new() -> Analyzer<'a> {
        Analyzer {
            diagnostics: Vec::new(),
            resolve: enum_resolves(),
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn run(&mut self, program: &'a Program) {
        for item in &program.items {
            self.run_item(item)
        }
    }

    pub fn run_item(&mut self, item: &'a Item) {
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

    pub fn run_argument(&mut self, element: &Element, arg: &'a Argument) {
        let Some(attrs) = self.resolve.get(&element.tag.value) else {
            return;
        };

        let Some(ty) = attrs.get(&arg.key.value) else {
            return;
        };

        let Some((_, expr)) = &arg.value else {
            if let Type::Empty = ty {

            } else {
                self.diagnostics.push(Diagnostic::Message(format!("Expected value for attribute '{}'", arg.key.value), arg.as_span(), DiagnosticLevel::Error))
            }

            return;
        };

        let ty = ty.clone();
        let expr_ty = self.run_expression(&expr);

        if !expr_ty.matches(&ty) {
            self.diagnostics.push(Diagnostic::Message(
                format!(
                    "Unexpected value of type `{}` for attribute `{}`",
                    expr_ty, arg.key.value
                ),
                expr.as_span(),
                DiagnosticLevel::Error,
            ));
        }
    }

    pub fn run_expression(&mut self, expr: &'a Expression) -> Type<'a> {
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
