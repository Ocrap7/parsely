use std::fmt::Debug;

use parsely_diagnostics::{Diagnostic, DiagnosticLevel};
use parsely_lexer::{
    tokens::{self, Bracket, Group, GroupBracket, Token},
    AsSpan, Span, Tok,
};

use crate::{
    dummy::Dummy,
    expression::{Argument, BinOp, Expression, ExpressionKind, Path},
    types::{Type, TypeKind},
    Brackets, NodeId, Parens, Parse, Punctuation, Result,
};

pub const MAX_SKIP: usize = 10;

macro_rules! skip_next {
    ($stream:expr => $tok:pat) => {{
        let mut res = false;
        for _ in 0..MAX_SKIP {
            match $stream.peek()? {
                $tok => {
                    res = true;
                    break;
                }
                _ => $stream.increment(),
            }
        }

        res
    }};
}

#[macro_export]
macro_rules! simple_attempt {
    [@mtok [ident] as $t:ident] => { Token::Ident($t) };
    [@mtok [ident]] => { Token::Ident(_) };
    [@tok [ident]] => { tokens::Ident };

    [@mtok [lit_string] as $t:ident] => { Token::String($t) };
    [@mtok [lit_string]] => { Token::String(_) };
    [@tok [lit_string]] => { tokens::String };

    [@mtok [lit_int] as $t:ident] => { Token::Int($t) };
    [@mtok [lit_int]] => { Token::Int(_) };
    [@tok [lit_int]] => { tokens::Int };

    [@mtok [lit_float] as $t:ident] => { Token::Float($t) };
    [@mtok [lit_float]] => { Token::Float(_) };
    [@tok [lit_float]] => { tokens::Float };

    [@mtok [shebang] as $t:ident] => { Token::Shebang($t) };
    [@mtok [shebang]] => { Token::Shebang(_) };
    [@tok [shebang]] => { tokens::Shebang };

    [@mtok [=] as $t:ident] => { Tok![enum = as $t] };
    [@mtok [=]] => { Tok![enum =] };
    [@tok [=]] => { Tok![=] };

    [@mtok [;] as $t:ident] => { Tok![enum ; as $t] };
    [@mtok [;]] => { Tok![enum ;] };
    [@tok [;]] => { Tok![;] };

    [@mtok [:] as $t:ident] => { Tok![enum : as $t] };
    [@mtok [:]] => { Tok![enum :] };
    [@tok [:]] => { Tok![:] };

    [@mtok [=>] as $t:ident] => { Tok![enum => as $t] };
    [@mtok [=>]] => { Tok![enum =>] };
    [@tok [=>]] => { Tok![=>] };

    [@mtok [.] as $t:ident] => { Tok![enum . as $t] };
    [@mtok [.]] => { Tok![enum .] };
    [@tok [.]] => { Tok![.] };

    [@mtok $tok:ident as $t:ident] => { Tok![enum $tok as $t] };
    [@mtok $tok:ident] => { Tok![enum $tok ] };
    [@tok $tok:ident] => { Tok![$tok] };

    [@str [$t:tt]] => {stringify!($t)};
    [@str $t:ident] => {stringify!($t)};

    ($stream:expr => $tok:tt) => {
        match ($stream.peek(), $stream.peekn(1)) {
            // Token found
            (Ok(simple_attempt![@mtok $tok as tok]), _) => $stream.next_ref(tok),
            // Single deletion case
            (Ok(full_tok), Ok(simple_attempt![@mtok $tok as tok])) => {
                let tok = tok.clone();

                $stream.push_diagnostic(parsely_diagnostics::Diagnostic::UnexpectedToken {
                    found: full_tok.clone(),
                    expected: simple_attempt![@str $tok].to_string(),
                });

                tok
            }
            // Single insertion
            (Ok(tok), _) => {
                let token = <simple_attempt![@tok $tok]>::new_dummy($stream.current_position());

                $stream.push_diagnostic(parsely_diagnostics::Diagnostic::UnexpectedToken {
                    found: tok.clone(),
                    expected: simple_attempt![@str $tok].to_string(),
                });

                token
            }
            // EOF
            (Err(_), _) => {
                $stream.push_diagnostic(parsely_diagnostics::Diagnostic::UnexpectedToken {
                    found: $stream.last().clone(),
                    expected: simple_attempt![@str $tok].to_string(),
                });

                <simple_attempt![@tok $tok]>::new_dummy($stream.current_position())
            }
        }
    };
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
}

impl Pattern {
    pub fn expect_ident(&self) -> &tokens::Ident {
        match &self.kind {
            PatternKind::Ident(id) => id,
            _ => panic!("Expected ident!"),
        }
    }

    pub fn visit_idents(&self, f: &mut impl FnMut(&tokens::Ident, &Pattern)) {
        match &self.kind {
            PatternKind::Ident(ident) => f(ident, self),
            PatternKind::Array(arr) => {
                for e in arr.value.iter() {
                    e.visit_idents(f)
                }
            }
            PatternKind::Tuple(arr) => {
                for e in arr.value.iter() {
                    e.visit_idents(f)
                }
            }
            PatternKind::Or(arr) => {
                for e in arr.iter() {
                    e.visit_idents(f)
                }
            }
            PatternKind::UnionVarient(_, _, Some(pat)) => pat.1.visit_idents(f),
            _ => (),
        }
    }
}

impl AsSpan for Pattern {
    fn as_span(&self) -> parsely_lexer::Span {
        self.kind.as_span()
    }
}

impl Parse for Pattern {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        Pattern::parse_op(stream)
    }
}

impl Pattern {
    fn parse_op(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let rstr = stream.current_point();
        let pat = Pattern::parse_primary(stream);

        match (pat, stream.peek()) {
            (Ok(_), Ok(Tok![enum |])) => {
                let id = stream.next_id();
                stream.restore(rstr);
                let kind = stream.parse().map(PatternKind::Or)?;

                Ok(Pattern { id, kind })
            }
            (Ok(p), _) => Ok(p),
            (Err(e), _) => {
                stream.restore(rstr);

                Err(e)
            }
        }
    }

    fn parse_primary(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let id = stream.next_id();
        let kind = match stream.peek()? {
            Tok![enum _] => stream.parse().map(PatternKind::Wild)?,
            Token::Group(Group {
                bracket: GroupBracket::Paren,
                ..
            }) => stream.parse().map(PatternKind::Tuple)?,
            Token::Group(Group {
                bracket: GroupBracket::Bracket,
                ..
            }) => stream.parse().map(PatternKind::Array)?,
            Tok![enum .] => PatternKind::UnionVarient(
                simple_attempt!(stream => [.]),
                stream.parse()?,
                if let Ok(Tok![enum :]) = stream.peek() {
                    Some((simple_attempt!(stream => [:]), stream.parse()?))
                } else {
                    None
                },
            ),
            Token::Ident(_) => stream.parse().map(PatternKind::Ident)?,
            _ => stream.parse().map(PatternKind::Expression)?,
        };

        Ok(Pattern { id, kind })
    }
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Poison,
    Wild(Tok![_]),
    Ident(tokens::Ident),
    Or(Punctuation<Pattern, Tok![|]>),
    Tuple(Parens<Punctuation<Pattern, Tok![,]>>),
    Array(Brackets<Punctuation<Pattern, Tok![,]>>),
    UnionVarient(Tok![.], tokens::Ident, Option<(Tok![:], Box<Pattern>)>),
    Expression(Box<Expression>),
}

impl AsSpan for PatternKind {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            PatternKind::Poison => Span::EMPTY,
            PatternKind::Wild(w) => w.as_span(),
            PatternKind::Ident(w) => w.as_span(),
            PatternKind::Or(w) => w.as_span(),
            PatternKind::Tuple(w) => w.as_span(),
            PatternKind::Array(w) => w.as_span(),
            PatternKind::Expression(w) => w.as_span(),
            PatternKind::UnionVarient(a, b, c) => {
                Span::join_all([a.as_span(), b.as_span(), c.as_span()])
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub id: NodeId,
    pub const_tok: Option<Tok![const]>,
    pub star: Option<Tok![*]>,
    pub pattern: Pattern,
    pub colon_tok: Tok![:],
    pub ty: Type,
    pub default: Option<(Tok![=], Box<Expression>)>,
}

impl AsSpan for Parameter {
    fn as_span(&self) -> Span {
        Span::join_all([
            self.star.as_span(),
            self.pattern.as_span(),
            self.colon_tok.as_span(),
            self.ty.as_span(),
            self.default.as_span(),
        ])
    }
}

impl Parse for Parameter {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let const_tok = if let Ok(Tok![enum const as c]) = stream.peek() {
            Some(stream.next_ref(c))
        } else {
            None
        };

        let star = if let Ok(Tok![enum * as star]) = stream.peek() {
            Some(stream.next_ref(star))
        } else {
            None
        };

        let pattern = if let Ok(pat) = stream.parse() {
            pat
        } else {
            skip_next!(stream => Tok![enum :]);
            Pattern {
                id: stream.next_id(),
                kind: PatternKind::Poison,
            }
        };
        let colon_tok = simple_attempt!(stream => [:]);

        Ok(Parameter {
            id: stream.next_id(),
            const_tok,
            star,
            pattern,
            colon_tok,
            ty: stream.parse()?,
            default: if let Ok(Tok![enum =]) = stream.peek() {
                Some((stream.parse()?, stream.parse()?))
            } else {
                None
            },
        })
    }
}

pub type Parameters = Parens<Punctuation<Parameter, Tok![,]>>;

#[derive(Debug, Clone)]
pub struct FunctionBinding {
    pub let_tok: Tok![let],
    pub const_tok: Option<Tok![const]>,
    pub inline_tok: Option<Tok![inline]>,
    pub internal_tok: Option<Tok![internal]>,
    pub ident: tokens::Ident,
    pub parameters: Parameters,
    pub ret_type: Option<(Tok![:], Box<Type>)>,
    pub eq_tok: Tok![=],
    pub value: Vec<Item>,
    pub semi_tok: Tok![;],
}

impl Parse for FunctionBinding {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let let_tok = simple_attempt!(stream => let);

        let const_tok = if let Tok![enum const as mutable] = stream.peek()? {
            Some(stream.next_ref(mutable))
        } else {
            None
        };

        let internal_tok = if let Tok![enum internal as mutable] = stream.peek()? {
            Some(stream.next_ref(mutable))
        } else {
            None
        };

        let inline_tok = if let Tok![enum inline as mutable] = stream.peek()? {
            Some(stream.next_ref(mutable))
        } else {
            None
        };

        let ident = simple_attempt!(stream => [ident]);
        let parameters = stream.parse()?;

        let ret_type = if let Ok(Tok![enum :]) = stream.peek() {
            Some((stream.parse()?, stream.parse()?))
        } else {
            None
        };

        let eq_tok = simple_attempt!(stream => [=]);
        let value =
            crate::parse_vec_to_terminator::<_, Tok![;]>(stream).expect("Should never error");
        let semi_tok = simple_attempt!(stream => [;]);

        Ok(FunctionBinding {
            let_tok,
            const_tok,
            inline_tok,
            internal_tok,
            ident,
            parameters,
            ret_type,
            eq_tok,
            value,
            semi_tok,
        })
    }
}

impl AsSpan for FunctionBinding {
    fn as_span(&self) -> parsely_lexer::Span {
        self.let_tok.as_span().join(self.semi_tok.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct ValueBinding {
    pub let_tok: Tok![let],

    pub mut_tok: Option<Tok![mut]>,
    pub pattern: Pattern,
    pub ty_annotation: Option<(Tok![:], Box<Type>)>,
    pub init: Option<(Tok![=], Box<Expression>)>,
}

impl Parse for ValueBinding {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let let_tok = simple_attempt!(stream => let);
        let mut_tok = if let Tok![enum mut as mutable] = stream.peek()? {
            Some(stream.next_ref(mutable))
        } else {
            None
        };

        let pattern = if let Ok(pat) = stream.parse() {
            pat
        } else {
            skip_next!(stream => Tok![enum =]);
            Pattern {
                id: stream.next_id(),
                kind: PatternKind::Poison,
            }
        };

        let ty_annotation = if let Ok(Tok![enum :]) = stream.peek() {
            Some((
                simple_attempt!(stream => [:]),
                stream.parse().expect("Should never fail"),
            ))
        } else {
            None
        };

        let init = if let Ok(Tok![enum =]) = stream.peek() {
            Some((
                simple_attempt!(stream => [=]),
                stream.parse().expect("Should never faile"),
            ))
        } else {
            None
        };

        Ok(ValueBinding {
            let_tok,
            mut_tok,
            pattern,
            ty_annotation,
            init,
        })
    }
}

impl AsSpan for ValueBinding {
    fn as_span(&self) -> parsely_lexer::Span {
        Span::join_all([
            self.let_tok.as_span(),
            self.mut_tok.as_span(),
            self.pattern.as_span(),
            self.ty_annotation.as_span(),
            self.init.as_span(),
        ])
    }
}

#[derive(Debug, Clone)]
pub enum Binding {
    Function(FunctionBinding),
    Value(ValueBinding),
}

impl Parse for Binding {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        if let Token::Group(tokens::Group {
            bracket: GroupBracket::Paren,
            ..
        }) = stream.peekn(2)?
        {
            stream.parse().map(Binding::Function)
        } else if let Tok![enum const] | Tok![enum inline] | Tok![enum internal] =
            stream.peekn(1)?
        {
            stream.parse().map(Binding::Function)
        } else {
            stream.parse().map(Binding::Value)
        }
    }
}

impl AsSpan for Binding {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Binding::Value(val) => val.as_span(),
            Binding::Function(fun) => fun.as_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Union {
    pub tag_type: Option<(Type, Tok![of])>,
    pub values: Punctuation<UnionVarient, Tok![|]>,
}

impl AsSpan for Union {
    fn as_span(&self) -> parsely_lexer::Span {
        self.values.as_span()
    }
}

impl Parse for Union {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        Ok(Union {
            tag_type: None,
            values: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum UnionVarientKind {
    Named(tokens::Ident),
    Typed(tokens::Ident, Tok![:], Type),
}

#[derive(Debug, Clone)]
pub struct UnionVarient {
    pub id: NodeId,
    pub kind: UnionVarientKind,
    pub index: Option<(Tok![=], Box<Expression>)>,
}

impl AsSpan for UnionVarient {
    fn as_span(&self) -> parsely_lexer::Span {
        match &self.kind {
            UnionVarientKind::Named(t) => t.as_span().join_saturated(self.index.as_span()),
            UnionVarientKind::Typed(a, b, c) => {
                Span::join_all([a.as_span(), b.as_span(), c.as_span(), self.index.as_span()])
            }
        }
    }
}

impl Parse for UnionVarient {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let id = stream.next_id();

        match (stream.peek()?, stream.peekn(1)) {
            (Token::Ident(_), Ok(Tok![enum :])) => Ok(UnionVarient {
                id,
                kind: UnionVarientKind::Typed(stream.parse()?, stream.parse()?, stream.parse()?),
                index: if let Ok(Tok![enum =]) = stream.peek() {
                    Some((
                        stream.parse()?,
                        Box::new(Expression::parse_primary_expression(stream)?),
                    ))
                } else {
                    None
                },
            }),
            _ => Ok(UnionVarient {
                id,
                kind: UnionVarientKind::Named(stream.parse()?),
                index: if let Ok(Tok![enum =]) = stream.peek() {
                    Some((
                        stream.parse()?,
                        Box::new(Expression::parse_primary_expression(stream)?),
                    ))
                } else {
                    None
                },
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeBinding {
    Type(Type),
    Union(Union),
    Named(Punctuation<Parameter, Tok![,]>),
    Function(Vec<Item>),
}

impl AsSpan for TypeBinding {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            TypeBinding::Type(t) => t.as_span(),
            TypeBinding::Union(t) => t.as_span(),
            TypeBinding::Named(t) => t.as_span(),
            TypeBinding::Function(t) => t.as_span(),
        }
    }
}

impl Parse for TypeBinding {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let restore = stream.current_point();

        match (stream.peek(), stream.peekn(1)) {
            // Case of pure enum varient
            (Ok(Token::Ident(last)), Ok(Tok![enum |])) => {
                let last = stream.next_ref(last);

                let un = Punctuation::parse_first(
                    stream,
                    Some(UnionVarient {
                        id: stream.next_id(),
                        kind: UnionVarientKind::Named(last),
                        index: None,
                    }),
                )?;

                Ok(TypeBinding::Union(Union {
                    tag_type: None,
                    values: un,
                }))
            }
            // Case of either enum varient with type, or aggregate type
            (Ok(Token::Ident(last)), Ok(Tok![enum :])) => {
                let last = stream.next_ref(last);

                let colon_tok = stream.parse()?;
                let ty = stream.parse()?;

                let default = if let Ok(Tok![enum = ]) = stream.peek() {
                    Some((
                        stream.parse()?,
                        Box::new(Expression::parse_primary_expression(stream)?),
                    ))
                } else {
                    None
                };

                // Case of union
                if let Ok(Tok![enum |]) = stream.peek() {
                    let first = UnionVarient {
                        id: stream.next_id(),
                        kind: UnionVarientKind::Typed(last, colon_tok, ty),
                        index: default,
                    };
                    let un = Punctuation::parse_first(stream, Some(first))?;

                    Ok(TypeBinding::Union(Union {
                        tag_type: None,
                        values: un,
                    }))
                } else {
                    // case of aggregate
                    let first = Parameter {
                        id: stream.next_id(),
                        const_tok: None,
                        pattern: Pattern {
                            id: stream.next_id(),
                            kind: PatternKind::Ident(last),
                        },
                        colon_tok,
                        star: None,
                        default,
                        ty,
                    };
                    let fields = Punctuation::parse_first(stream, Some(first))?;

                    Ok(TypeBinding::Named(fields))
                }
            }
            // Case of pure enum varient with index value
            (Ok(Token::Ident(last)), Ok(Tok![enum =])) => {
                let last = stream.next_ref(last);

                let first = UnionVarient {
                    id: stream.next_id(),
                    kind: UnionVarientKind::Named(last),
                    index: if let Ok(Tok![enum =]) = stream.peek() {
                        Some((stream.parse()?, stream.parse()?))
                    } else {
                        None
                    },
                };

                let un = Punctuation::parse_first(stream, Some(first))?;

                Ok(TypeBinding::Union(Union {
                    tag_type: None,
                    values: un,
                }))
            }
            // Case of explicitly tagged union
            (Ok(Token::Ident(_)), Ok(Tok![enum of])) => {
                let ty = stream.parse()?;
                let of_tok = stream.parse()?;

                let un = stream.parse()?;

                Ok(TypeBinding::Union(Union {
                    tag_type: Some((ty, of_tok)),
                    values: un,
                }))
            }
            // Default is a simple type alias
            _ => {
                // stream.restore(restore);
                let ty = stream.parse::<Type>();

                match (ty, stream.peek()) {
                    (Ok(ty), Ok(Tok![enum ;])) => Ok(TypeBinding::Type(ty)),
                    (Ok(ty), _) => {
                        let first = Item {
                            id: stream.next_id(),
                            attributes: Vec::new(),
                            docs: None,
                            export_tok: None,
                            kind: ItemKind::Expression(Box::new(Expression {
                                id: stream.next_id(),
                                kind: ExpressionKind::Type(Box::new(ty)),
                            })),
                        };

                        // TODO: don't make this so stupid
                        let mut items =
                            stream.ty_ctx(crate::parse_vec_to_terminator::<_, Tok![;]>)?;
                        items.insert(0, first);

                        Ok(TypeBinding::Function(items))
                    }
                    _ => {
                        stream.restore(restore);

                        let items = stream.ty_ctx(crate::parse_vec_to_terminator::<_, Tok![;]>)?;

                        Ok(TypeBinding::Function(items))
                    }
                }

                // stream.parse().map(TypeBinding::Type)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub type_tok: Tok![type],
    pub ident: tokens::Ident,
    pub params: Option<Parameters>,
    pub eq_tok: Tok![=],
    pub ty: TypeBinding,
    pub semi_tok: Tok![;],
}

impl Parse for TypeAlias {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let type_tok = simple_attempt!(stream => type);
        let ident = simple_attempt!(stream => [ident]);

        let params = if let Ok(Token::Group(Group {
            bracket: GroupBracket::Paren,
            ..
        })) = stream.peek()
        {
            Some(stream.parse()?)
        } else {
            None
        };

        let eq_tok = simple_attempt!(stream => [=]);
        let ty = stream.parse().expect("Should never fail");
        let semi_tok = simple_attempt!(stream => [;]);

        Ok(TypeAlias {
            type_tok,
            ident,
            params,
            eq_tok,
            ty,
            semi_tok,
        })
    }
}

impl AsSpan for TypeAlias {
    fn as_span(&self) -> parsely_lexer::Span {
        self.type_tok.as_span().join(self.ty.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub shebang_tok: tokens::Shebang,
    pub value: Brackets<Expression>,
}

impl Parse for Attribute {
    fn parse(stream: &'_ mut crate::ParseStream) -> crate::Result<Self> {
        let shebang_tok = simple_attempt!(stream => [shebang]);
        let value = match stream.peek()? {
            Token::Group(Group {
                bracket: GroupBracket::Bracket,
                ..
            }) => stream.parse()?,
            tok => {
                if shebang_tok.0.is_dummy() {
                    return Err(Diagnostic::UnexpectedToken {
                        found: tok.clone(),
                        expected: "attribute".to_string(),
                    });
                } else {
                    Brackets {
                        brackets: Bracket {
                            span: stream.current_position().join(stream.current_position()),
                        },
                        value: Box::new(Expression {
                            id: stream.next_id(),
                            kind: ExpressionKind::Poison,
                        }),
                    }
                }
            }
        };

        Ok(Attribute { shebang_tok, value })
    }
}

impl AsSpan for Attribute {
    fn as_span(&self) -> parsely_lexer::Span {
        self.shebang_tok.as_span().join(self.value.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub ret_tok: Tok![return],
    pub expr: Option<Box<Expression>>,
    pub semi_tok: Option<Tok![;]>,
}

impl AsSpan for Return {
    fn as_span(&self) -> Span {
        self.ret_tok.as_span().join_saturated(self.expr.as_span())
    }
}

impl Parse for Return {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let ret_tok = simple_attempt!(stream => return);

        if let Ok(Tok![enum ; as semi]) = stream.peek() {
            Ok(Return {
                ret_tok,
                expr: None,
                semi_tok: Some(stream.next_ref(semi)),
            })
        } else {
            Ok(Return {
                ret_tok,
                expr: Some(stream.parse()?),
                semi_tok: None,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Break {
    pub break_tok: Tok![break],
    pub expr: Option<Box<Expression>>,
    pub semi_tok: Option<Tok![;]>,
}

impl AsSpan for Break {
    fn as_span(&self) -> Span {
        self.break_tok.as_span().join_saturated(self.expr.as_span())
    }
}

impl Parse for Break {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let break_tok = simple_attempt!(stream => break);

        if let Ok(Tok![enum ; as semi]) = stream.peek() {
            Ok(Break {
                break_tok,
                expr: None,
                semi_tok: Some(stream.next_ref(semi)),
            })
        } else {
            Ok(Break {
                break_tok,
                expr: Some(stream.parse()?),
                semi_tok: None,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub cont_tok: Tok![continue],
    pub expr: Option<Box<Expression>>,
    pub semi_tok: Option<Tok![;]>,
}

impl AsSpan for Continue {
    fn as_span(&self) -> Span {
        self.cont_tok.as_span().join_saturated(self.expr.as_span())
    }
}

impl Parse for Continue {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let cont_tok = simple_attempt!(stream => continue);

        if let Ok(Tok![enum ; as semi]) = stream.peek() {
            Ok(Continue {
                cont_tok,
                expr: None,
                semi_tok: Some(stream.next_ref(semi)),
            })
        } else {
            Ok(Continue {
                cont_tok,
                expr: Some(stream.parse()?),
                semi_tok: None,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub mod_tok: Tok![module],
    pub ident: Option<tokens::Ident>,
    pub parameters: Option<Parameters>,
    pub body: Option<(Tok![=], Vec<Item>)>,
    pub semi_tok: Tok![;],
}

impl AsSpan for Module {
    fn as_span(&self) -> Span {
        Span::join_all([
            self.mod_tok.as_span(),
            self.ident.as_span(),
            self.parameters.as_span(),
            self.body.as_span(),
            self.semi_tok.as_span(),
        ])
    }
}

impl Parse for Module {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let mod_tok = simple_attempt!(stream => module);

        let ident = if let Ok(Token::Ident(i)) = stream.peek() {
            Some(stream.next_ref(i))
        } else {
            None
        };

        let parameters = if let Ok(Token::Group(Group {
            bracket: GroupBracket::Paren,
            ..
        })) = stream.peek()
        {
            Some(stream.parse()?)
        } else {
            None
        };

        let body = if let Ok(Tok![enum =]) = stream.peek() {
            Some((
                stream.parse()?,
                crate::parse_vec_to_terminator::<_, Tok![;]>(stream)?,
            ))
        } else {
            None
        };

        let semi_tok = simple_attempt!(stream => [;]);

        Ok(Module {
            mod_tok,
            ident,
            parameters,
            body,
            semi_tok,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub import_tok: Tok![import],
    pub path: Path,
}

impl AsSpan for Import {
    fn as_span(&self) -> Span {
        self.import_tok
            .as_span()
            .join_saturated(self.path.as_span())
    }
}

impl Parse for Import {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        Ok(Import {
            import_tok: simple_attempt!(stream => import),
            path: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Attribute(Attribute),
    Binding(Binding),
    TypeAlias(TypeAlias),
    Expression(Box<Expression>),
    Return(Return),
    Break(Break),
    Continue(Continue),

    Module(Module),
    Import(Import),

    Poison,
}

#[derive(Debug, Clone)]
pub struct Item {
    pub id: NodeId,

    pub attributes: Vec<Attribute>,
    pub docs: Option<Vec<tokens::DocComment>>,
    pub export_tok: Option<Tok![export]>,
    pub kind: ItemKind,
}

impl Parse for Item {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let id = stream.next_id();
        let attributes = if let Token::Shebang(_) = stream.peek()? {
            stream.parse().unwrap_or(Vec::new())
        } else {
            Vec::new()
        };

        let mut docs = None;

        while let Ok(tok) = stream.peek() {
            match tok {
                Token::DocComment(doc) => {
                    let docs = docs.get_or_insert(Vec::new());
                    docs.push(stream.next_ref(doc));
                }
                _ => break,
            }
        }

        let export_tok = if let Tok![enum export as ex] = stream.peek()? {
            Some(stream.next_ref(ex))
        } else {
            None
        };

        let kind = match stream.peek()? {
            Tok![enum return] => stream.parse().map(ItemKind::Return)?,
            Tok![enum break] => stream.parse().map(ItemKind::Break)?,
            Tok![enum continue] => stream.parse().map(ItemKind::Continue)?,
            Tok![enum let] => stream.parse().map(ItemKind::Binding)?,
            Tok![enum type] => stream.parse().map(ItemKind::TypeAlias)?,
            Tok![enum module] => stream.parse().map(ItemKind::Module)?,
            Tok![enum import] => stream.parse().map(ItemKind::Import)?,
            Token::Shebang(_) => stream.parse().map(ItemKind::Attribute)?,
            _ => stream.try_parse().map(ItemKind::Expression).or_else(|e| {
                stream.push_diagnostic(Diagnostic::Message(
                    format!(
                        "Expected an item but found token `{}`",
                        stream
                            .peek()
                            .map(|t| t.to_string())
                            .unwrap_or("<eof>".to_string())
                    ),
                    stream
                        .peek()
                        .map(|t| t.as_span())
                        .unwrap_or(stream.current_position().join(stream.current_position())),
                    DiagnosticLevel::Error,
                ));

                return Err(e);
            })?,
        };

        Ok(Item {
            id,
            attributes,
            docs,
            export_tok,
            kind,
        })
    }
}

impl AsSpan for Item {
    fn as_span(&self) -> parsely_lexer::Span {
        match &self.kind {
            ItemKind::Attribute(attr) => attr.as_span(),
            ItemKind::Binding(bind) => bind.as_span(),
            ItemKind::TypeAlias(ta) => ta.as_span(),
            ItemKind::Expression(ta) => ta.as_span(),
            ItemKind::Return(ta) => ta.as_span(),
            ItemKind::Break(ta) => ta.as_span(),
            ItemKind::Continue(ta) => ta.as_span(),
            ItemKind::Module(ta) => ta.as_span(),
            ItemKind::Import(ta) => ta.as_span(),
            ItemKind::Poison => parsely_lexer::Span::EMPTY,
        }
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Debug;

    use parsely_diagnostics::DiagnosticModuleFmt;
    use parsely_lexer::{Lexer, Position};

    use crate::{program::Program, ParseStream};

    use super::*;

    #[test]
    fn test_item() {
        // let toks = lx("let  = ;");

        // let mut diags = Vec::new();
        // let mut acutal = TokenIter::from(toks);
        // let mut expected = TokenIter::from(vec![
        //     tokens::Let::new_dummy(Position::EMPTY).into(),
        //     tokens::Ident::new_dummy(Position::EMPTY).into(),
        //     tokens::Assign::new_dummy(Position::EMPTY).into(),
        //     tokens::Semi::new_dummy(Position::EMPTY).into(),
        // ]);

        // let fixes = parse_me(&mut diags, &mut acutal, &mut expected, 0, None);
        // let item = prs("#![] let CONST = 45; let func = add x0 = x1 + x1, sub x5 = x1 - x2 ;");

        // println!("{item:#?}");
    }
}
