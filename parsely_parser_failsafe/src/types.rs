use parsely_diagnostics::{Diagnostic, DiagnosticLevel, Result};
use parsely_lexer::{
    tokens::{Bracket, Group, GroupBracket, Token},
    AsSpan, Tok,
};

use crate::{
    expression::{Argument, Expression, Path},
    Brackets, NodeId, Parens, Parse, Punctuation,
};

#[derive(Debug, Clone)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Type(Tok![type]),
    Named(Path),
    Ref(Box<Type>, RefPart),
    IndexRef(Box<Type>, Brackets<RefPart>),
    Slice(Brackets<(Option<Tok![mut]>, Type)>),
    Array(Brackets<ArrayInner>),
    Optional(Box<Type>, Tok![?]),
    Arguments(Box<Type>, Parens<Punctuation<Argument, Tok![,]>>),
    Tuple(Punctuation<Type, Tok![*]>),
}

impl AsSpan for TypeKind {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            TypeKind::Type(n) => n.as_span(),
            TypeKind::Named(n) => n.as_span(),
            TypeKind::Ref(a, b) => a.as_span().join(b.as_span()),
            TypeKind::IndexRef(a, b) => a.as_span().join(b.as_span()),
            TypeKind::Slice(a) => a.as_span(),
            TypeKind::Array(a) => a.as_span(),
            TypeKind::Optional(a, b) => a.as_span().join(b.as_span()),
            TypeKind::Arguments(a, b) => a.as_span().join(b.as_span()),
            TypeKind::Tuple(a) => a.as_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RefPart {
    pub ref_tok: Tok![&],
    pub mut_tok: Option<Tok![mut]>,
}

impl AsSpan for RefPart {
    fn as_span(&self) -> parsely_lexer::Span {
        if let Some(ref mt) = self.mut_tok {
            self.ref_tok.as_span().join(mt.as_span())
        } else {
            self.ref_tok.as_span()
        }
    }
}

impl Parse for RefPart {
    fn parse(stream: &'_ mut crate::ParseStream) -> parsely_diagnostics::Result<Self> {
        Ok(RefPart {
            ref_tok: stream.parse()?,
            mut_tok: {
                if let Ok(Tok![enum mut]) = stream.peek() {
                    Some(stream.parse()?)
                } else {
                    None
                }
            },
        })
    }
}

#[derive(Debug, Clone)]
pub struct ArrayInner {
    pub ty: Box<Type>,
    pub colon: Tok![:],
    pub size: Box<Expression>,
}

impl Parse for ArrayInner {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        Ok(ArrayInner {
            ty: stream.parse()?,
            colon: stream.parse()?,
            size: stream.parse()?,
        })
    }
}

impl Parse for Type {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let first = Type::parse_type(stream)?;

        if let Ok(Tok![enum *]) = stream.peek() {
            let kind = Punctuation::parse_first_with(stream, Some(first), Type::parse_type)
                .map(TypeKind::Tuple)?;

            Ok(Type {
                id: stream.next_id(),
                kind,
            })
        } else {
            Ok(first)
        }
    }
}

impl Type {
    fn parse_type(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let mut base = match stream.peek()? {
            Token::Group(Group {
                bracket: GroupBracket::Bracket,
                open,
                close,
                tokens,
                ..
            }) => {
                stream.increment();

                let mut stream = stream.fork(open.join(*close), tokens);

                let has_mut = if let Ok(Tok![enum mut as token]) = stream.peek() {
                    Some(stream.next_ref(token))
                } else {
                    None
                };

                let base = stream.parse()?;

                if let Ok(Tok![enum :]) = stream.peek() {
                    if let Some(mut_tok) = has_mut {
                        stream.push_diagnostic(Diagnostic::Message(
                            format!("Token `mut` was not expected in this context"),
                            mut_tok.as_span(),
                            DiagnosticLevel::Error,
                        ))
                    }

                    let kind = TypeKind::Array(Brackets {
                        brackets: Bracket {
                            span: open.join(*close),
                        },
                        value: Box::new(ArrayInner {
                            ty: base,
                            colon: stream.parse()?,
                            size: stream.parse()?,
                        }),
                    });

                    Type {
                        id: stream.next_id(),
                        kind,
                    }
                } else {
                    let kind = TypeKind::Slice(Brackets {
                        brackets: Bracket {
                            span: open.join(*close),
                        },
                        value: Box::new((has_mut, *base)),
                    });

                    Type {
                        id: stream.next_id(),
                        kind,
                    }
                }
            }
            _ => Type::parse_primary(stream)?,
        };

        while let Ok(tok) = stream.peek() {
            match tok {
                Token::Group(Group {
                    bracket: GroupBracket::Paren,
                    ..
                }) => {
                    let id = stream.next_id();

                    base = Type {
                        id,
                        kind: TypeKind::Arguments(Box::new(base), stream.parse()?),
                    }
                }
                Token::Group(Group {
                    bracket: GroupBracket::Bracket,
                    ..
                }) => {
                    let id = stream.next_id();

                    base = Type {
                        id,
                        kind: TypeKind::IndexRef(Box::new(base), stream.parse()?),
                    }
                }
                Tok![enum &] => {
                    let id = stream.next_id();
                    base = Type {
                        id,
                        kind: TypeKind::Ref(Box::new(base), stream.parse()?),
                    };
                }
                Tok![enum ?] => {
                    let id = stream.next_id();
                    base = Type {
                        id,
                        kind: TypeKind::Optional(Box::new(base), stream.parse()?),
                    };
                }
                _ => break,
            }
        }

        Ok(base)
    }
}

impl Type {
    fn parse_primary(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        match stream.peek()? {
            Token::Type(_) => {
                let kind = stream.parse().map(TypeKind::Type)?;

                Ok(Type {
                    id: stream.next_id(),
                    kind,
                })
            }
            Token::Ident(_) => {
                let kind = stream.parse().map(TypeKind::Named)?;

                Ok(Type {
                    id: stream.next_id(),
                    kind,
                })
            }
            tok => Err(Diagnostic::Message(
                format!("Expected named type!"),
                tok.as_span(),
                DiagnosticLevel::Error,
            )),
        }
    }
}

impl AsSpan for Type {
    fn as_span(&self) -> parsely_lexer::Span {
        self.kind.as_span()
    }
}

#[cfg(test)]
mod test {

    use crate::testing::*;
    use parsely_lexer::{span, tokens, Lexer};

    use super::*;

    #[test]
    fn test_named() {
        let ty: Type = prs("int32");

        match &ty.kind {
            TypeKind::Named(Path {
                segments: Punctuation { items, last },
            }) => match (&items[..], last) {
                ([], Some(box tokens::Ident { value: p, .. })) if p == "int32" => {}
                _ => {
                    println!("{ty:#?}");
                    panic!("Type doesn't match! {ty:#?}")
                }
            },
            _ => {
                println!("{ty:#?}");
                panic!("Type doesn't match! {ty:#?}")
            }
        }

        assert_eq!(ty.as_span(), span!(0:0-5));
    }

    #[test]
    fn test_ref() {
        let ty: Type = prs("int8&");

        match &ty.kind {
            TypeKind::Ref(
                box Type {
                    kind:
                        TypeKind::Named(Path {
                            segments: Punctuation { items, last },
                            ..
                        }),
                    ..
                },
                RefPart { mut_tok: None, .. },
            ) => match (&items[..], last) {
                ([], Some(box tokens::Ident { value: p, .. })) if p == "int8" => {}
                _ => {
                    println!("{ty:#?}");
                    panic!("Type doesn't match! {ty:#?}")
                }
            },
            _ => {
                println!("{ty:#?}");
                panic!("Type doesn't match! {ty:#?}")
            }
        }

        assert_eq!(ty.as_span(), span!(0:0-5));
    }

    #[test]
    fn test_ref_mut() {
        let ty: Type = prs("bool&mut");

        match &ty.kind {
            TypeKind::Ref(
                box Type {
                    kind:
                        TypeKind::Named(Path {
                            segments: Punctuation { items, last },
                            ..
                        }),
                    ..
                },
                RefPart {
                    mut_tok: Some(_), ..
                },
            ) => match (&items[..], last) {
                ([], Some(box tokens::Ident { value: p, .. })) if p == "bool" => {}
                _ => {
                    println!("{ty:#?}");
                    panic!("Type doesn't match! {ty:#?}")
                }
            },
            _ => {
                println!("{ty:#?}");
                panic!("Type doesn't match! {ty:#?}")
            }
        }

        assert_eq!(ty.as_span(), span!(0:0-8));
    }

    #[test]
    fn test_opt() {
        let ty: Type = prs("float64?");

        match &ty.kind {
            TypeKind::Optional(
                box Type {
                    kind:
                        TypeKind::Named(Path {
                            segments: Punctuation { items, last },
                            ..
                        }),
                    ..
                },
                _,
            ) => match (&items[..], last) {
                ([], Some(box tokens::Ident { value: p, .. })) if p == "float64" => {}
                _ => {
                    println!("{ty:#?}");
                    panic!("Type doesn't match! {ty:#?}")
                }
            },
            _ => {
                println!("{ty:#?}");
                panic!("Type doesn't match! {ty:#?}")
            }
        }

        assert_eq!(ty.as_span(), span!(0:0-8));
    }
}
