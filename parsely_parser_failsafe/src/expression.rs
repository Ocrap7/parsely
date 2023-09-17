use parsely_diagnostics::{Diagnostic, DiagnosticLevel, Result};
use parsely_lexer::{
    tokens::{self, Group, GroupBracket, Paren, Token},
    AsSpan, Span, Tok,
};

use crate::{
    dummy::Dummy,
    item::{Item, Pattern},
    simple_attempt,
    types::Type,
    Braces, Brackets, NodeId, Parens, Parse, ParseStream, Punctuation,
};

#[derive(Debug, Clone)]
pub enum Literal {
    Int(LiteralInt),
    Float(LiteralFloat),
    String(LiteralString),
    Bool(tokens::Bool),
    None(tokens::Nones),
}

impl AsSpan for Literal {
    fn as_span(&self) -> parsely_lexer::Span {
        match self {
            Literal::Int(l) => l.value.as_span(),
            Literal::Float(l) => l.value.as_span(),
            Literal::String(l) => l.value.as_span(),
            Literal::Bool(l) => l.as_span(),
            Literal::None(l) => l.as_span(),
        }
    }
}

impl Parse for Literal {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        match stream.peek_expected("literal")? {
            Token::Int(_) => stream.parse().map(|s| Literal::Int(s)),
            Token::Float(_) => stream.parse().map(|s| Literal::Float(s)),
            Token::String(_) => stream.parse().map(|s| Literal::String(s)),
            Token::Bool(_) => stream.parse().map(|s| Literal::Bool(s)),
            Token::Nones(_) => stream.parse().map(|s| Literal::None(s)),
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: "literal".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralInt {
    pub value: tokens::Int,
}

impl Parse for LiteralInt {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        // let value = simple_attempt!(stream => [lit_int]);

        // Ok(LiteralInt { value })
        match stream.peek()? {
            Token::Int(i) => Ok(LiteralInt {
                value: stream.next_ref(i),
            }),
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::Int::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralFloat {
    pub value: tokens::Float,
}

impl Parse for LiteralFloat {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        // let value = simple_attempt!(stream => [lit_float]);

        // Ok(LiteralFloat { value })
        match stream.peek()? {
            Token::Float(i) => Ok(LiteralFloat {
                value: stream.next_ref(i),
            }),
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::Float::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LiteralString {
    pub value: tokens::String,
}

impl Parse for LiteralString {
    fn parse(stream: &'_ mut crate::ParseStream) -> Result<Self> {
        // let value = simple_attempt!(stream => [lit_string]);

        // Ok(LiteralString { value })

        match stream.peek()? {
            Token::String(i) => Ok(LiteralString {
                value: stream.next_ref(i),
            }),
            tok => Err(Diagnostic::UnexpectedToken {
                found: tok.clone(),
                expected: tokens::String::NAME.to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Punctuation<tokens::Ident, Tok![.]>,
}

impl AsSpan for Path {
    fn as_span(&self) -> Span {
        self.segments.as_span()
    }
}

impl Parse for Path {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        Ok(Path {
            segments: stream.parse().expect("Shouldnt fail"),
        })
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Poison,

    Literal(Literal),
    /// std.heap.List
    Path(Path),
    /// foo = 4
    Assign(Assign),
    /// [1, 2, 3]
    ArrayInit(ArrayInit),
    /// (10, false, "Hello")
    TupleInit(TupleInit),
    /// { name: value }
    StructInit(StructInit),

    /// (2 + 3)
    Parens(Parens<Expression>),
    BinOp(BinOp),
    /// &value, &mut value
    AddrOf(AddrOf),
    /// *reference_value
    Deref(Deref),
    /// First is the base, second is the index
    Index(Box<Expression>, Brackets<Expression>),
    /// foo(1, true)
    Call(Call),

    /// if cond then ... else ...
    If(If),
    /// loop do ...
    /// loop cond do ...
    /// loop pat in iter do ...
    Loop(Loop),
    /// match value with ... ;
    Match(Match),
    /// const do ... ;
    ConstDo(ConstDo),

    Type(Box<Type>),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub id: NodeId,
    pub kind: ExpressionKind,
}

impl AsSpan for Expression {
    fn as_span(&self) -> parsely_lexer::Span {
        match &self.kind {
            ExpressionKind::Poison => parsely_lexer::Span::EMPTY,
            ExpressionKind::Literal(l) => l.as_span(),
            ExpressionKind::BinOp(b) => b.as_span(),
            ExpressionKind::Path(i) => i.as_span(),
            ExpressionKind::Assign(i) => i.as_span(),
            ExpressionKind::Parens(i) => i.value.as_span(),
            ExpressionKind::Index(i, ind) => i.as_span().join(ind.as_span()),
            ExpressionKind::ArrayInit(i) => i.as_span(),
            ExpressionKind::TupleInit(i) => i.as_span(),
            ExpressionKind::StructInit(i) => i.as_span(),
            ExpressionKind::AddrOf(i) => i.as_span(),
            ExpressionKind::Deref(i) => i.as_span(),
            ExpressionKind::Call(i) => i.as_span(),
            ExpressionKind::If(i) => i.as_span(),
            ExpressionKind::Loop(i) => i.as_span(),
            ExpressionKind::Match(i) => i.as_span(),
            ExpressionKind::ConstDo(i) => i.as_span(),
            ExpressionKind::Type(i) => i.as_span(),
        }
    }
}

impl Expression {
    pub fn parse_primary_expression(stream: &'_ mut ParseStream) -> Result<Expression> {
        let id = stream.next_id();
        let kind = match stream.peek_expected("parenthesis, identifier, or literal") {
            Ok(Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Paren,
                tokens,
                open,
                close,
                ..
            })) => {
                let span = open.join(*close);
                stream.increment();
                let mut stream = stream.fork(open.join(*close), tokens);

                match (stream.parse::<Expression>(), stream.peek()) {
                    (Ok(expr), Ok(Tok![enum,])) => {
                        let punct = Punctuation::parse_first(&mut stream, Some(expr))?;

                        ExpressionKind::TupleInit(TupleInit {
                            elements: Parens {
                                parens: Paren { span },
                                value: Box::new(punct),
                            },
                        })
                    }
                    (Ok(parens), Err(_)) => ExpressionKind::Parens(Parens {
                        parens: Paren { span },
                        value: Box::new(parens),
                    }),
                    _ => {
                        stream.push_diagnostic(Diagnostic::Message(
                            format!("Expected expression inside parenthesis"),
                            span,
                            DiagnosticLevel::Error,
                        ));

                        ExpressionKind::Parens(Parens {
                            parens: tokens::Paren { span },
                            value: Box::new(Expression {
                                id: stream.next_id(),
                                kind: ExpressionKind::Poison,
                            }),
                        })
                    }
                }
            }
            Ok(Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Bracket,
                ..
            })) if !stream.in_ty_ctx() => stream.parse().map(ExpressionKind::ArrayInit)?,
            Ok(Token::Group(tokens::Group {
                bracket: tokens::GroupBracket::Brace,
                ..
            })) if !stream.in_ty_ctx() => ExpressionKind::StructInit(StructInit {
                ty: None,
                elements: stream.parse()?,
            }),
            Ok(Token::Ident(_)) => {
                let path = stream.parse()?;

                // If braces follow, this is a struct initializer
                if let Ok(Token::Group(Group {
                    bracket: GroupBracket::Brace,
                    ..
                })) = stream.peek()
                {
                    ExpressionKind::StructInit(StructInit {
                        ty: Some(path),
                        elements: stream.parse()?,
                    })
                } else {
                    ExpressionKind::Path(path)
                }
            }
            Ok(Tok![enum if]) => stream.parse().map(ExpressionKind::If)?,
            Ok(Tok![enum loop]) => stream.parse().map(ExpressionKind::Loop)?,
            Ok(Tok![enum match]) => stream.parse().map(ExpressionKind::Match)?,
            Ok(Tok![enum const]) => stream.parse().map(ExpressionKind::ConstDo)?,
            Ok(_) => {
                // We try to parse a type if we are in a type context
                let res = if stream.in_ty_ctx() {
                    let ty = stream.try_parse();
                    ty.map(|f| ExpressionKind::Type(Box::new(f))).ok()
                } else {
                    None
                };

                if let Some(res) = res {
                    res
                } else {
                    let res = stream.parse().map(ExpressionKind::Literal);

                    match res {
                        Ok(expr) => expr,
                        Err(diag) => {
                            stream.push_diagnostic(diag);

                            ExpressionKind::Poison
                        }
                    }
                }
            }
            Err(e) => {
                stream.push_diagnostic(e);

                ExpressionKind::Poison
            }
        };

        Ok(Expression { id, kind })
    }
}

impl Parse for Expression {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        let binop = BinOp::parse_binop(stream, 0);

        println!("S{:?}", stream.peek());
        let expr = match (stream.peek(), binop) {
            (
                Ok(
                    Tok![enum =]
                    | Tok![enum +=]
                    | Tok![enum -=]
                    | Tok![enum *=]
                    | Tok![enum /=]
                    | Tok![enum %=]
                    | Tok![enum <<=]
                    | Tok![enum >>=]
                    | Tok![enum &=]
                    | Tok![enum ^=]
                    | Tok![enum |=],
                ),
                Ok(left),
            ) => {
                let id = stream.next_id();

                Ok(Expression {
                    id,
                    kind: ExpressionKind::Assign(Assign {
                        left: Box::new(left),
                        op: stream.next()?,
                        right: stream.parse()?,
                    }),
                })
            }
            (_, binop) => binop,
        };

        expr
    }
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub left: Box<Expression>,
    pub op: tokens::Token,
    pub right: Box<Expression>,
}

impl AsSpan for BinOp {
    fn as_span(&self) -> parsely_lexer::Span {
        let left = self.left.as_span();
        let right = self.right.as_span();
        if left == Span::EMPTY {
            self.op.as_span().join(right)
        } else if right == Span::EMPTY {
            left.join(self.op.as_span())
        } else {
            left.join(right)
        }
    }
}

impl BinOp {
    fn parse_binop(stream: &'_ mut ParseStream, last_prec: usize) -> Result<Expression> {
        let mut left = match stream.peek()? {
            tok @ Tok![enum &] if BinOp::pre_precedence(tok) >= last_prec => {
                let prec = BinOp::pre_precedence(tok);
                let id = stream.next_id();

                let addr = AddrOf {
                    ref_tok: stream.parse()?,
                    mut_tok: {
                        if let Tok![enum mut] = stream.peek()? {
                            Some(stream.parse()?)
                        } else {
                            None
                        }
                    },
                    expr: Box::new(BinOp::parse_binop(stream, prec)?),
                };

                Expression {
                    id,
                    kind: ExpressionKind::AddrOf(addr),
                }
            }
            tok @ Tok![enum *] if BinOp::pre_precedence(tok) >= last_prec => {
                let prec = BinOp::pre_precedence(tok);
                let id = stream.next_id();

                let addr = Deref {
                    deref_tok: stream.parse()?,
                    expr: Box::new(BinOp::parse_binop(stream, prec)?),
                };

                Expression {
                    id,
                    kind: ExpressionKind::Deref(addr),
                }
            }
            _ => {
                let lhs = Expression::parse_primary_expression(stream)?;

                lhs
            }
        };

        while stream.has_next() && !stream.peek_eof() {
            let op = stream.peek().cloned().expect("stream should have token");

            match op {
                Token::Group(Group {
                    bracket: GroupBracket::Bracket,
                    open,
                    close,
                    ..
                }) => {
                    let id = stream.next_id();
                    let span = open.join(close);
                    if let Ok(ind) = stream.parse() {
                        left = Expression {
                            id,
                            kind: ExpressionKind::Index(Box::new(left), ind),
                        }
                    } else {
                        left = Expression {
                            id,
                            kind: ExpressionKind::Index(
                                Box::new(left),
                                Brackets {
                                    brackets: tokens::Bracket { span },
                                    value: Box::new(Expression {
                                        id: stream.next_id(),
                                        kind: ExpressionKind::Poison,
                                    }),
                                },
                            ),
                        };

                        continue;
                    }
                }
                Token::Group(Group {
                    bracket: GroupBracket::Paren,
                    ..
                }) => {
                    let id = stream.next_id();
                    left = Expression {
                        id,
                        kind: ExpressionKind::Call(Call {
                            expr: Box::new(left),
                            args: stream.parse()?,
                        }),
                    };

                    continue;
                }
                _ => (),
            }

            let prec = BinOp::precedence(&op);

            if prec <= last_prec || prec == 0 {
                break;
            }

            let id = stream.next_id();

            let op = stream.next().expect("operator should exist");

            let right = BinOp::parse_binop(stream, prec).expect("This should never error");

            left = Expression {
                id,
                kind: ExpressionKind::BinOp(BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }),
            };
        }

        while stream.has_next() && !stream.peek_eof() {
            let op = stream.peek().cloned().expect("stream should have token");

            let prec = BinOp::post_precedence(&op);

            if prec <= last_prec || prec == 0 {
                break;
            }

            let id = stream.next_id();

            let op = stream.next().expect("operator should exist");

            match op {
                Tok![enum ds as op] => {
                    left = Expression {
                        id,
                        kind: ExpressionKind::Deref(Deref {
                            deref_tok: op,
                            expr: Box::new(left),
                        }),
                    };
                }
                _ => (),
            }
        }

        Ok(left)
    }

    fn post_precedence(op: &tokens::Token) -> usize {
        match op {
            tokens::Tok![enum ds] => 140,
            _ => 0
        }
    }

    fn precedence(op: &tokens::Token) -> usize {
        match op {
            tokens::Tok![enum *] | tokens::Tok![enum /] | tokens::Tok![enum %] => 130,
            tokens::Tok![enum +] | tokens::Tok![enum -] => 120,
            tokens::Tok![enum <<] | tokens::Tok![enum >>] => 110,
            tokens::Tok![enum <]
            | tokens::Tok![enum >]
            | tokens::Tok![enum <=]
            | tokens::Tok![enum >=] => 100,
            tokens::Tok![enum ==] | tokens::Tok![enum !=] => 90,
            tokens::Tok![enum &] => 80,
            tokens::Tok![enum ^] => 70,
            tokens::Tok![enum |] => 60,
            tokens::Tok![enum &&] => 50,
            tokens::Tok![enum ||] => 40,
            tokens::Tok![enum ..] | tokens::Tok![enum ..=] => 45,
            // tokens::Tok![enum =]
            // | tokens::Tok![enum +=]
            // | tokens::Tok![enum -=]
            // | tokens::Tok![enum *=]
            // | tokens::Tok![enum /=]
            // | tokens::Tok![enum %=]
            // | tokens::Tok![enum <<=]
            // | tokens::Tok![enum >>=]
            // | tokens::Tok![enum &=]
            // | tokens::Tok![enum ^=]
            // | tokens::Tok![enum |=] => 30,
            _ => 0,
        }
    }

    fn pre_precedence(op: &tokens::Token) -> usize {
        match op {
            Tok![enum &] => 140,
            _ => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayInit {
    pub elements: Brackets<Punctuation<Expression, tokens::Tok![,]>>,
}

impl AsSpan for ArrayInit {
    fn as_span(&self) -> parsely_lexer::Span {
        self.elements.value.as_span()
    }
}

impl Parse for ArrayInit {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        Ok(ArrayInit {
            elements: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct TupleInit {
    pub elements: Parens<Punctuation<Expression, tokens::Tok![,]>>,
}

impl AsSpan for TupleInit {
    fn as_span(&self) -> parsely_lexer::Span {
        self.elements.value.as_span()
    }
}

impl Parse for TupleInit {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        Ok(TupleInit {
            elements: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct StructInit {
    pub ty: Option<Path>,
    pub elements: Braces<Punctuation<StructInitField, Tok![,]>>,
}

impl AsSpan for StructInit {
    fn as_span(&self) -> Span {
        self.ty.as_span().join_saturated(self.elements.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct StructInitField {
    pub name: tokens::Ident,
    pub init: Option<(Tok![:], Box<Expression>)>,
}

impl Parse for StructInitField {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        Ok(StructInitField {
            name: simple_attempt!(stream => [ident]),
            init: {
                if let Ok(Tok![enum :]) = stream.peek() {
                    Some((stream.parse()?, stream.parse()?))
                } else {
                    None
                }
            },
        })
    }
}

#[derive(Debug, Clone)]
pub struct AddrOf {
    pub ref_tok: Tok![&],
    pub mut_tok: Option<Tok![mut]>,
    pub expr: Box<Expression>,
}

impl AsSpan for AddrOf {
    fn as_span(&self) -> Span {
        self.ref_tok.as_span().join(self.expr.as_span())
    }
}

impl Parse for AddrOf {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        Ok(AddrOf {
            ref_tok: stream.parse()?,
            mut_tok: {
                if let Tok![enum mut] = stream.peek()? {
                    Some(stream.parse()?)
                } else {
                    None
                }
            },
            expr: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Deref {
    pub expr: Box<Expression>,
    pub deref_tok: Tok![ds],
}

impl AsSpan for Deref {
    fn as_span(&self) -> Span {
        self.deref_tok.as_span().join(self.expr.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub left: Box<Expression>,
    pub op: Token,
    pub right: Box<Expression>,
}

impl Assign {
    /// Get the binary equivelent of the operator
    ///
    /// >>= becomes >>
    pub fn bin_op_token(&self) -> Token {
        match self.op {
            Tok![enum +=] => Token::Plus(tokens::Plus(self.op.as_span())),
            Tok![enum -=] => Token::Minus(tokens::Minus(self.op.as_span())),
            Tok![enum *=] => Token::Star(tokens::Star(self.op.as_span())),
            Tok![enum /=] => Token::Slash(tokens::Slash(self.op.as_span())),
            Tok![enum %=] => Token::Rem(tokens::Rem(self.op.as_span())),

            Tok![enum <<=] => Token::LeftShift(tokens::LeftShift(self.op.as_span())),
            Tok![enum >>=] => Token::RightShift(tokens::RightShift(self.op.as_span())),

            Tok![enum &=] => Token::And(tokens::And(self.op.as_span())),
            Tok![enum |=] => Token::Or(tokens::Or(self.op.as_span())),
            Tok![enum ^=] => Token::Xor(tokens::Xor(self.op.as_span())),
            _ => panic!(),
        }
    }
}

impl AsSpan for Assign {
    fn as_span(&self) -> Span {
        Span::join_all([self.left.as_span(), self.op.as_span(), self.right.as_span()])
    }
}

#[derive(Debug, Clone)]
pub enum Argument {
    Expression(Expression),
    Type(Type),
    Named(tokens::Ident, Tok![:], Box<Argument>),
}

impl AsSpan for Argument {
    fn as_span(&self) -> Span {
        match self {
            Argument::Type(t) => t.as_span(),
            Argument::Expression(t) => t.as_span(),
            Argument::Named(a, b, c) => Span::join_all([a.as_span(), b.as_span(), c.as_span()]),
        }
    }
}

impl Parse for Argument {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        if let Ok(Tok![enum :]) = stream.peekn(1) {
            Ok(Argument::Named(
                stream.parse()?,
                simple_attempt!(stream => [:]),
                stream.parse()?,
            ))
        } else {
            stream.parse().map(Argument::Expression)
        }
    }
}

pub type Arguments = Parens<Punctuation<Argument, Tok![,]>>;

#[derive(Debug, Clone)]
pub struct Call {
    pub expr: Box<Expression>,
    pub args: Arguments,
}

impl AsSpan for Call {
    fn as_span(&self) -> Span {
        self.expr.as_span().join_saturated(self.args.as_span())
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub if_tok: Tok![if],
    pub expr: Box<Expression>,
    pub then_tok: Tok![then],
    pub body: Vec<Item>,
    pub else_clause: Option<Else>,
    pub semi_tok: Tok![;],
}

impl AsSpan for If {
    fn as_span(&self) -> Span {
        Span::join_all([
            self.if_tok.as_span(),
            self.expr.as_span(),
            self.then_tok.as_span(),
            self.body.as_span(),
            self.else_clause.as_span(),
        ])
    }
}

impl Parse for If {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        let if_tok = simple_attempt!(stream => if);
        let expr = stream.parse()?;
        let then_tok = simple_attempt!(stream => then);
        let body = crate::parse_vec_to_one_of2::<_, Tok![;], Tok![else]>(stream)?;

        let else_clause = if let Ok(Tok![enum else]) = stream.peek() {
            Some(stream.parse()?)
        } else {
            None
        };

        let semi_tok = simple_attempt!(stream => [;]);

        Ok(If {
            if_tok,
            expr,
            then_tok,
            body,
            else_clause,
            semi_tok,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Else {
    pub else_tok: Tok![else],
    pub body: Vec<Item>,
}

impl AsSpan for Else {
    fn as_span(&self) -> Span {
        Span::join_all([self.else_tok.as_span(), self.body.as_span()])
    }
}

impl Parse for Else {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        Ok(Else {
            else_tok: simple_attempt!(stream => else),
            body: crate::parse_vec_to_terminator::<_, Tok![;]>(stream)?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum LoopKind {
    Empty,
    Condition(Box<Expression>),
    Iter(Pattern, Tok![in], Box<Expression>),
}

impl AsSpan for LoopKind {
    fn as_span(&self) -> Span {
        match self {
            LoopKind::Empty => Span::EMPTY,
            LoopKind::Condition(cond) => cond.as_span(),
            LoopKind::Iter(a, b, c) => Span::join_all([a.as_span(), b.as_span(), c.as_span()]),
        }
    }
}

impl Parse for LoopKind {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        if let Tok![enum do] = stream.peek()? {
            return Ok(LoopKind::Empty);
        }

        let rstr = stream.current_point();
        let pattern = stream.try_parse::<Pattern>();

        if let (Ok(p), Ok(Tok![enum in])) = (pattern, stream.peek()) {
            Ok(LoopKind::Iter(p, stream.parse()?, stream.parse()?))
        } else {
            stream.restore(rstr);

            Ok(LoopKind::Condition(stream.parse()?))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub loop_tok: Tok![loop],
    pub cond: LoopKind,
    pub do_tok: Tok![do],
    pub body: Vec<Item>,
    pub semi_tok: Tok![;],
}

impl AsSpan for Loop {
    fn as_span(&self) -> Span {
        Span::join_all([
            self.loop_tok.as_span(),
            self.cond.as_span(),
            self.do_tok.as_span(),
            self.body.as_span(),
        ])
    }
}

impl Parse for Loop {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        let loop_tok = simple_attempt!(stream => loop);
        let cond = stream.parse()?;
        let do_tok = simple_attempt!(stream => do);
        let body = crate::parse_vec_to_terminator::<_, Tok![;]>(stream)?;
        let semi_tok = simple_attempt!(stream => [;]);

        Ok(Loop {
            loop_tok,
            cond,
            do_tok,
            body,
            semi_tok,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Match {
    pub match_tok: Tok![match],
    pub expr: Box<Expression>,
    pub with_tok: Tok![with],
    pub arms: Punctuation<MatchArm, Tok![,]>,
    pub semi_tok: Tok![;],
}

impl AsSpan for Match {
    fn as_span(&self) -> Span {
        Span::join_all([
            self.match_tok.as_span(),
            self.expr.as_span(),
            self.with_tok.as_span(),
            self.arms.as_span(),
            self.semi_tok.as_span(),
        ])
    }
}

impl Parse for Match {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        let match_tok = simple_attempt!(stream => match);
        let expr = stream.parse().map(Box::new)?;
        let with_tok = simple_attempt!(stream => with);
        let arms = stream.parse()?;
        let semi_tok = simple_attempt!(stream => [;]);

        Ok(Match {
            match_tok,
            expr,
            with_tok,
            arms,
            semi_tok,
        })
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub arrow: Tok![=>],
    pub body: Vec<Item>,
}

impl AsSpan for MatchArm {
    fn as_span(&self) -> Span {
        Span::join_all([
            self.pattern.as_span(),
            self.arrow.as_span(),
            self.body.as_span(),
        ])
    }
}

impl Parse for MatchArm {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        Ok(MatchArm {
            pattern: stream.parse()?,
            arrow: simple_attempt!(stream => [=>]),
            body: crate::parse_vec_to_one_of2::<_, Tok![;], Tok![,]>(stream)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ConstDo {
    pub const_tok: Tok![const],
    pub do_tok: Tok![do],
    pub body: Vec<Item>,
    pub semi_tok: Tok![;],
}

impl AsSpan for ConstDo {
    fn as_span(&self) -> Span {
        Span::join_all([
            self.const_tok.as_span(),
            self.do_tok.as_span(),
            self.body.as_span(),
            self.semi_tok.as_span(),
        ])
    }
}

impl Parse for ConstDo {
    fn parse(stream: &'_ mut ParseStream) -> Result<Self> {
        let const_tok = simple_attempt!(stream => const);
        let do_tok = simple_attempt!(stream => do);
        let body = crate::parse_vec_to_terminator::<_, Tok![;]>(stream)?;
        let semi_tok = simple_attempt!(stream => [;]);

        Ok(ConstDo {
            const_tok,
            do_tok,
            body,
            semi_tok,
        })
    }
}

#[cfg(test)]
mod test {

    use crate::testing::*;
    use parsely_lexer::{span, Lexer};

    use super::*;

    #[test]
    fn test_simple() {
        let expr: Expression = prs("3 + (4 * 5) - 6");
        match expr {
            Expression {
                kind:
                    ExpressionKind::BinOp(BinOp {
                        left:
                            box Expression {
                                kind:
                                    ExpressionKind::BinOp(BinOp {
                                        left:
                                            box Expression {
                                                kind:
                                                    ExpressionKind::Literal(Literal::Int(LiteralInt {
                                                        value: tokens::Int { value: 3, .. },
                                                    })),
                                                ..
                                            },
                                        op: Tok![enum +],
                                        right: box Expression {
                                            kind: ExpressionKind::Parens(Parens {
                                                value: box Expression {
                                                    kind: ExpressionKind::BinOp(BinOp {
                                            left: box Expression {
                                                kind:
                                                    ExpressionKind::Literal(Literal::Int(LiteralInt {
                                                        value: tokens::Int { value: 4, .. },
                                                    })),
                                                ..
                                            },
                                        op: Tok![enum *],
                                            right: box Expression {
                                                kind:
                                                    ExpressionKind::Literal(Literal::Int(LiteralInt {
                                                        value: tokens::Int { value: 5, .. },
                                                    })),
                                                ..
                                            },
                                                    }),
                                                    ..
                                                },
                                                ..
                                            }),
                                            ..
                                        },
                                    }),
                                ..
                            },
                        op: Tok![enum -],
                        right:
                            box Expression {
                                kind:
                                    ExpressionKind::Literal(Literal::Int(LiteralInt {
                                        value: tokens::Int { value: 6, .. },
                                    })),
                                ..
                            },
                    }),
                ..
            } => {}
            _ => {
                println!("{expr:#?}");
                panic!("Expression doesn't match! {expr:#?}")
            }
        };
    }

    #[test]
    fn test_path() {
        let expr: Expression = prs("std.heap.List");
        match &expr {
            Expression {
                kind:
                    ExpressionKind::Path(Path {
                        segments: Punctuation { items, last },
                    }),
                ..
            } => match (&items[..], last) {
                (
                    [(tokens::Ident { value: p1, .. }, _), (tokens::Ident { value: p2, .. }, _)],
                    Some(box tokens::Ident { value: p3, .. }),
                ) if p1 == "std" && p2 == "heap" && p3 == "List" => {}
                _ => {
                    println!("{expr:#?}");
                    panic!("Expression doesn't match! {expr:#?}")
                }
            },
            _ => {
                println!("{expr:#?}");
                panic!("Expression doesn't match! {expr:#?}")
            }
        };

        assert_eq!(expr.as_span(), span!(0:0-13));
    }
}
