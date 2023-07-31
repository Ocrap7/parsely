use parsely_lexer::tokens::{self, Group, GroupBracket, Token};

use crate::{statement::ArrayDimension, Parse, ParseError};

#[derive(Debug, Clone)]
pub enum Type {
    Empty,
    Int(TypeInt),
    Array(TypeArray),
    Slice(TypeArray),
    Str(tokens::Ident),
    Void(tokens::Void),
    Named(tokens::Ident),
}

impl Parse for Type {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let base = match stream.peek()? {
            tokens::Tok!(enum void as v) => Ok(Type::Void(stream.next_ref(v))),
            Token::Ident(ident) if &ident.value[..3] == "int" => stream.parse().map(Type::Int),
            Token::Ident(ident) if ident.value == "str" => stream.parse().map(Type::Str),
            Token::Ident(_) => stream.parse().map(Type::Named),
            found => Err(ParseError::UnexpectedToken {
                found: found.clone(),
                expected: "Type".to_string(),
            }),
        };

        base.and_then(|base| match stream.peek()? {
            Token::Group(Group {
                bracket: GroupBracket::Bracket,
                ..
            }) => Ok(Type::Array(TypeArray {
                element: Box::new(base),
                arrays: stream.parse()?,
            })),
            _ => Ok(base),
        })
    }
}

#[derive(Debug, Clone)]
pub struct TypeInt {
    pub size: usize,
    pub token: tokens::Ident,
}

impl Parse for TypeInt {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Ident(tok @ tokens::Ident { value, .. }) => {
                let (ipart, size_part) = value.split_at(3);
                if ipart != "int" {
                    return Err(ParseError::UnexpectedToken {
                        found: stream.peek()?.clone(),
                        expected: "int".to_string(),
                    });
                }

                Ok(TypeInt {
                    size: size_part.parse().map_err(|_| ParseError::UnexpectedSize {
                        found: size_part.to_string(),
                    })?,
                    token: stream.next_ref(tok),
                })
            }
            found => Err(ParseError::UnexpectedToken {
                found: found.clone(),
                expected: "Int Type".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeArray {
    pub element: Box<Type>,
    pub arrays: Vec<ArrayDimension>,
}

impl Parse for TypeArray {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(TypeArray {
            element: stream.parse()?,
            arrays: stream.parse()?,
        })
    }
}
