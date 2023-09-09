use std::fmt::Debug;

use parsely_diagnostics::{Diagnostic, DiagnosticLevel};
use parsely_lexer::{
    tokens::{self, Bracket, DocComment, Group, GroupBracket, Token},
    AsSpan, Tok,
};

use crate::{dummy::Dummy, expression::Expression, Brackets, Parens, Parse, Punctuation, Result, types::Type};

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
pub struct Parameter {
    pub name: tokens::Ident,
    pub colon_tok: Tok![:],
    pub ty: Type,
    pub default: Option<(Tok![=], Expression)>,
}

impl Parse for Parameter {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let name = simple_attempt!(stream => [ident]);
        let colon_tok = simple_attempt!(stream => [:]);

        Ok(Parameter {
            name,
            colon_tok,
            ty: stream.parse()?,
            default: if let Tok![enum =] = stream.peek()? {
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
    pub docs: Option<Vec<tokens::DocComment>>,
    pub export_tok: Option<Tok![export]>,
    pub let_tok: Tok![let],
    pub const_tok: Option<Tok![const]>,
    pub inline_tok: Option<Tok![inline]>,
    pub internal_tok: Option<Tok![internal]>,
    pub ident: tokens::Ident,
    pub parameters: Parameters,
    pub eq_tok: Tok![=],
    pub value: Vec<Item>,
    pub semi_tok: Tok![;],
}

impl FunctionBinding {
    fn parse_with_docs(
        stream: &'_ mut crate::ParseStream,
        docs: Option<Vec<DocComment>>,
    ) -> Result<Self> {
        let export_tok = if let Tok![enum export as mutable] = stream.peek()? {
            Some(stream.next_ref(mutable))
        } else {
            None
        };

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
        let eq_tok = simple_attempt!(stream => [=]);
        let value = stream.parse().expect("Should never error");
        let semi_tok = simple_attempt!(stream => [;]);

        Ok(FunctionBinding {
            docs,
            export_tok,
            let_tok,
            const_tok,
            inline_tok,
            internal_tok,
            ident,
            parameters,
            eq_tok,
            value,
            semi_tok,
        })
    }
}

impl Parse for FunctionBinding {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
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

        FunctionBinding::parse_with_docs(stream, docs)
    }
}

impl AsSpan for FunctionBinding {
    fn as_span(&self) -> parsely_lexer::Span {
        self.let_tok.as_span().join(self.semi_tok.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct ValueBinding {
    pub docs: Option<Vec<tokens::DocComment>>,
    pub export_tok: Option<Tok![export]>,
    pub let_tok: Tok![let],
    pub mut_tok: Option<Tok![mut]>,
    pub ident: tokens::Ident,
    pub eq_tok: Tok![=],
    pub value: Expression,
}

impl ValueBinding {
    fn parse_with_docs(
        stream: &'_ mut crate::ParseStream,
        docs: Option<Vec<DocComment>>,
    ) -> Result<Self> {
        let export_tok = if let Tok![enum export as mutable] = stream.peek()? {
            Some(stream.next_ref(mutable))
        } else {
            None
        };

        let let_tok = simple_attempt!(stream => let);
        let mut_tok = if let Tok![enum mut as mutable] = stream.peek()? {
            Some(stream.next_ref(mutable))
        } else {
            None
        };

        let ident = simple_attempt!(stream => [ident]);
        let eq_tok = simple_attempt!(stream => [=]);
        let value = stream.parse().expect("Should never error");

        Ok(ValueBinding {
            docs,
            export_tok,
            let_tok,
            mut_tok,
            ident,
            eq_tok,
            value,
        })
    }
}

impl Parse for ValueBinding {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
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

        ValueBinding::parse_with_docs(stream, docs)
    }
}

impl AsSpan for ValueBinding {
    fn as_span(&self) -> parsely_lexer::Span {
        self.let_tok.as_span().join(self.value.as_span())
    }
}

#[derive(Debug, Clone)]
pub enum Binding {
    Function(FunctionBinding),
    Value(ValueBinding),
}

impl Binding {
    fn parse_with_docs(
        stream: &'_ mut crate::ParseStream,
        docs: Option<Vec<DocComment>>,
    ) -> Result<Self> {
        if let Token::Group(tokens::Group {
            bracket: GroupBracket::Paren,
            ..
        }) = stream.peekn(2)?
        {
            FunctionBinding::parse_with_docs(stream, docs).map(Binding::Function)
        } else if let Tok![enum const] | Tok![enum inline] | Tok![enum internal] =
            stream.peekn(1)?
        {
            FunctionBinding::parse_with_docs(stream, docs).map(Binding::Function)
        } else {
            ValueBinding::parse_with_docs(stream, docs).map(Binding::Value)
        }
    }
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
pub struct TypeAlias {
    pub docs: Option<Vec<tokens::DocComment>>,
    pub type_tok: Tok![type],
    pub ident: tokens::Ident,
    pub params: Option<Parameters>,
    pub eq_tok: Tok![=],
    pub ty: Type,
}

impl TypeAlias {
    fn parse_with_docs(
        stream: &'_ mut crate::ParseStream,
        docs: Option<Vec<DocComment>>,
    ) -> Result<Self> {
        let type_tok = simple_attempt!(stream => type);
        let ident = simple_attempt!(stream => [ident]);
        let params = stream.try_parse().ok();
        let eq_tok = simple_attempt!(stream => [=]);
        let ty = stream.parse().expect("Should never fail");

        Ok(TypeAlias {
            docs,
            type_tok,
            ident,
            params,
            eq_tok,
            ty,
        })
    }
}

impl Parse for TypeAlias {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
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

        TypeAlias::parse_with_docs(stream, docs)
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
                        value: Box::new(Expression::Poison),
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
pub enum Item {
    Attribute(Attribute),
    Binding(Binding),
    TypeAlias(TypeAlias),
    Poison,
}

impl Parse for Item {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        match stream.peek()? {
            Tok![enum let] | Token::DocComment(_) => stream.parse().map(Item::Binding),
            Tok![enum type] | Token::DocComment(_) => stream.parse().map(Item::TypeAlias),
            _ => stream.parse().map(Item::Attribute).or_else(|e| {
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
            }),
        }
    }
}

impl AsSpan for Item {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Item::Attribute(attr) => attr.as_span(),
            Item::Binding(bind) => bind.as_span(),
            Item::TypeAlias(ta) => ta.as_span(),
            Item::Poison => parsely_lexer::Span::EMPTY,
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

    fn lx(st: &str) -> Vec<Token> {
        Lexer::run(st)
    }

    fn prs(st: &str) -> Program {
        let toks = lx(st);
        println!("toks: {toks:#?}");

        let program = Program::new("poo.par", st.to_string(), toks);
        let (program, diags) = program.parse().unwrap();

        let fmt = DiagnosticModuleFmt(&diags, &program);
        print!("{fmt}");

        program
    }

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
        let item = prs("#![] let CONST = 45; let func = add x0 = x1 + x1, sub x5 = x1 - x2 ;");

        println!("{item:#?}");
    }
}
