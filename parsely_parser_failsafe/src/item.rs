use std::fmt::Debug;

use parsely_diagnostics::{Diagnostic, DiagnosticLevel};
use parsely_lexer::{
    tokens::{self, Bracket, Group, GroupBracket, Token},
    AsSpan, Tok,
};

use crate::{dummy::Dummy, expression::Expression, Brackets, Parse, Punctuation, Result};

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
pub struct Op {
    pub op: tokens::Ident,
    pub args: Expression,
}

impl Parse for Op {
    fn parse(stream: &'_ mut crate::ParseStream) -> crate::Result<Self> {
        let ident = match stream.peekn(1)? {
            Token::Ident(_) => stream.parse()?,
            tok => {
                stream.push_diagnostic(Diagnostic::UnexpectedToken {
                    found: tok.clone(),
                    expected: "instruction".to_string(),
                });
                tokens::Ident::new_dummy(stream.current_position())
            }
        };

        Ok(Op {
            op: ident,
            args: stream.parse().expect("Should never error"),
        })
    }
}

impl AsSpan for Op {
    fn as_span(&self) -> parsely_lexer::Span {
        self.op.as_span().join(self.args.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct OpList {
    pub list: Punctuation<Op, tokens::Comma>,
}

impl Parse for OpList {
    fn parse(stream: &'_ mut crate::ParseStream) -> crate::Result<Self> {
        Ok(OpList {
            list: Punctuation::parse_end_terminator::<Tok![;]>(stream)?,
        })
    }
}

impl AsSpan for OpList {
    fn as_span(&self) -> parsely_lexer::Span {
        self.list.as_span()
    }
}

#[derive(Debug, Clone)]
pub enum BoundValue {
    Expression(Expression),
    OpList(OpList),
}

impl Parse for BoundValue {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        let expr = stream.try_parse_with::<Expression>(|stream| {
            let expr = stream.parse::<Expression>().expect("Should not error");

            match (expr, stream.peek_expected("semicolon")?) {
                (expr, tokens::Tok![enum ;]) => Ok(expr),
                (expr, tokens::Tok![enum let]) => Ok(expr),
                (_, tok) => Err(Diagnostic::UnexpectedToken {
                    found: tok.clone(),
                    expected: "semicolon".to_string(),
                }),
            }
        });

        expr.map(BoundValue::Expression)
            .or_else(|_| stream.parse().map(BoundValue::OpList))
    }
}

impl AsSpan for BoundValue {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            BoundValue::Expression(expr) => expr.as_span(),
            BoundValue::OpList(op) => op.as_span(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub docs: Option<Vec<tokens::DocComment>>,
    pub let_tok: Tok![let],
    pub ident: tokens::Ident,
    pub eq_tok: Tok![=],
    pub value: BoundValue,
    pub semi_tok: Tok![;],
}

impl Parse for Binding {
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

        let let_tok = simple_attempt!(stream => let);
        let ident = simple_attempt!(stream => [ident]);
        let eq_tok = simple_attempt!(stream => [=]);
        let value = stream.parse().expect("Should never error");
        let semi_tok = simple_attempt!(stream => [;]);

        Ok(Binding {
            docs,
            let_tok,
            ident,
            eq_tok,
            value,
            semi_tok,
        })
    }
}

impl AsSpan for Binding {
    fn as_span(&self) -> parsely_lexer::Span {
        self.let_tok.as_span().join(self.semi_tok.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub shebang_tok: tokens::Shebang,
    pub value: Brackets<Expression>,
}

impl Parse for Attribute {
    fn parse(stream: &'_ mut crate::ParseStream) -> crate::Result<Self> {
        println!("{:?}", stream.peek());
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
    Poison,
}

impl Parse for Item {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        println!("{}", stream.index.get());
        match stream.peek()? {
            Tok![enum let] | Token::DocComment(_) => stream.parse().map(Item::Binding),
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

                // Ok(Item::Poison)
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
