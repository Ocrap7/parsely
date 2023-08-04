use parsely_lexer::tokens::{self, Token};

use crate::{typess, Parse, ParseError, Punctuation, PunctuationLast};

#[derive(Debug, Clone)]
pub enum Noun {
    Function(tokens::Function),
    Variable(tokens::Variable),
    Constant(tokens::Constant),
}

impl Noun {
    pub fn is_fn(&self) -> bool {
        matches!(self, Noun::Function(_))
    }
}

impl Parse for Noun {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Function(func) => Ok(Noun::Function(stream.next_ref(func))),
            Token::Variable(func) => Ok(Noun::Variable(stream.next_ref(func))),
            Token::Constant(func) => Ok(Noun::Constant(stream.next_ref(func))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "noun".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Indefinite {
    pub tok: tokens::A,
    pub noun: Noun,
}

impl Parse for Indefinite {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Indefinite {
            tok: stream.parse()?,
            noun: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Definite {
    pub tok: tokens::The,
    pub noun: Noun,
}

impl Parse for Definite {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Definite {
            tok: stream.parse()?,
            noun: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(tokens::Int),
    Float(tokens::Float),
    Bool(tokens::Bool),
    String(tokens::String),
}

impl Parse for Literal {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Int(i) => Ok(Literal::Int(stream.next_ref(i))),
            Token::Float(i) => Ok(Literal::Float(stream.next_ref(i))),
            Token::Bool(i) => Ok(Literal::Bool(stream.next_ref(i))),
            Token::String(i) => Ok(Literal::String(stream.next_ref(i))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "literal".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    pub the_tok: tokens::The,
    pub value_tok: tokens::Value,
    pub lit: Literal,
}

impl Parse for Value {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Value {
            the_tok: stream.parse()?,
            value_tok: stream.parse()?,
            lit: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ResultOf {
    pub the_tok: tokens::The,
    pub result: tokens::Result,
    pub of: tokens::Of,
    pub op: OpList,
}

impl Parse for ResultOf {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ResultOf {
            the_tok: stream.parse()?,
            result: stream.parse()?,
            of: stream.parse()?,
            op: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ValueOf {
    pub the_tok: tokens::The,
    pub value_tok: tokens::Value,
    pub of: tokens::Of,
    pub ident: tokens::Ident,
}

impl Parse for ValueOf {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ValueOf {
            the_tok: stream.parse()?,
            value_tok: stream.parse()?,
            of: stream.parse()?,
            ident: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Value(Value),
    ValueOf(ValueOf),
    Result(ResultOf),
}

impl Parse for Expression {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peekn(1)? {
            tokens::Tok![enum value] => match stream.peekn(2) {
                Ok(tokens::Tok![enum of]) => stream.parse().map(Expression::ValueOf),
                _ => stream.parse().map(Expression::Value),
            },
            tokens::Tok![enum result] => stream.parse().map(Expression::Result),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "expression".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinOperator {
    Add(tokens::Adding),
    Sub(tokens::Subtracting),
    Mult(tokens::Multiplying),
    Div(tokens::Dividing),
}

impl Parse for BinOperator {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Adding(a) => Ok(BinOperator::Add(stream.next_ref(a))),
            Token::Subtracting(a) => Ok(BinOperator::Sub(stream.next_ref(a))),
            Token::Multiplying(a) => Ok(BinOperator::Mult(stream.next_ref(a))),
            Token::Dividing(a) => Ok(BinOperator::Div(stream.next_ref(a))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "binary operator".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg(tokens::Negating),
}

impl Parse for UnaryOperator {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            Token::Negating(n) => Ok(UnaryOperator::Neg(stream.next_ref(n))),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "unary operator".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub op: BinOperator,
    pub left: Box<Expression>,
    pub and_tok: tokens::And,
    pub right: Box<Expression>,
}

impl Parse for BinOp {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(BinOp {
            op: stream.parse()?,
            left: stream.parse()?,
            and_tok: stream.parse()?,
            right: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    pub op: UnaryOperator,
    pub expr: Box<Expression>,
}

impl Parse for UnaryOp {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(UnaryOp {
            op: stream.parse()?,
            expr: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct OpBy {
    pub op: BinOperator,
    pub by: tokens::By,
    pub expr: Box<Expression>,
}

impl Parse for OpBy {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(OpBy {
            op: stream.parse()?,
            by: stream.parse()?,
            expr: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct UnaryImpl {
    pub op: UnaryOperator,
}

impl Parse for UnaryImpl {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(UnaryImpl {
            op: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    OpBy(OpBy),
    UnaryImpl(UnaryImpl),
}

#[derive(Debug, Clone)]
pub enum BinOrUnary {
    BinOp(BinOp),
    UnaryOp(UnaryOp),
}

impl Parse for BinOrUnary {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        if let Ok(op) = stream.parse() {
            Ok(BinOrUnary::BinOp(BinOp {
                op,
                left: stream.parse()?,
                and_tok: stream.parse()?,
                right: stream.parse()?,
            }))
        } else if let Ok(op) = stream.parse() {
            Ok(BinOrUnary::UnaryOp(UnaryOp {
                op,
                expr: stream.parse()?,
            }))
        } else {
            Err(ParseError::UnexpectedToken {
                found: stream.peek()?.clone(),
                expected: "operator".into(),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub enum ByOrImpl {
    OpBy(OpBy),
    UnaryImpl(UnaryImpl),
}

impl Parse for ByOrImpl {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        if let Ok(op) = stream.parse() {
            Ok(ByOrImpl::OpBy(OpBy {
                op,
                by: stream.parse()?,
                expr: stream.parse()?,
            }))
        } else if let Ok(op) = stream.parse() {
            Ok(ByOrImpl::UnaryImpl(UnaryImpl { op }))
        } else {
            Err(ParseError::UnexpectedToken {
                found: stream.peek()?.clone(),
                expected: "operator".into(),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct OpList {
    pub first: Option<BinOrUnary>,
    pub comma: Option<tokens::Tok![,]>,
    pub rest: PunctuationLast<ByOrImpl, tokens::Tok![,], tokens::Tok![and]>,
}

impl OpList {
    pub fn iter_operation(&self) -> impl Iterator<Item = Operation> + '_ {
        self.first
            .as_ref()
            .map(|op| match op {
                BinOrUnary::BinOp(bin) => Operation::BinOp(bin.clone()),
                BinOrUnary::UnaryOp(unary) => Operation::UnaryOp(unary.clone()),
            })
            .into_iter()
            .chain(self.rest.iter().map(|op| match op {
                ByOrImpl::OpBy(onby) => Operation::OpBy(onby.clone()),
                ByOrImpl::UnaryImpl(un) => Operation::UnaryImpl(un.clone()),
            }))
    }
}

impl Parse for OpList {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let first = stream.parse().ok();

        match stream.peek() {
            Ok(Token::Comma(c)) if first.is_some() => {
                let comma = stream.next_ref(c);
                let rest: PunctuationLast<ByOrImpl, tokens::Tok![,], tokens::Tok![and]> =
                    stream.parse()?;

                if rest.len() <= 0 {
                    return Err(ParseError::UnexpectedToken {
                        found: Token::Comma(comma.clone()),
                        expected: "operations".into(),
                    });
                }

                Ok(OpList {
                    first,
                    comma: Some(comma),
                    rest: rest,
                })
            }
            _ => Ok(OpList {
                first,
                comma: None,
                rest: PunctuationLast::empty(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Definition(DefinitionAction),
    Execute(Execute),
}

impl Parse for Statement {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum defines] => Ok(Statement::Definition(stream.parse()?)),
            tokens::Tok![enum executes] => Ok(Statement::Execute(stream.parse()?)),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "statement".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Execute {
    pub exe_tok: tokens::Executes,
    pub what: Definite,
    pub called_tok: tokens::Called,
    pub ident: tokens::Ident,
}

impl Parse for Execute {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Execute {
            exe_tok: stream.parse()?,
            what: stream.parse()?,
            called_tok: stream.parse()?,
            ident: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct BlockList {
    pub statements: Punctuation<Statement, tokens::Then>,
}

impl Parse for BlockList {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(BlockList {
            statements: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct MutableInit {
    pub starts_tok: tokens::Starts,
    pub with_tok: tokens::With,
    pub value: Expression,
}

impl Parse for MutableInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(MutableInit {
            starts_tok: stream.parse()?,
            with_tok: stream.parse()?,
            value: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ConstantInit {
    pub contains_tok: tokens::Contains,
    pub value: Expression,
}

impl Parse for ConstantInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(ConstantInit {
            contains_tok: stream.parse()?,
            value: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum VariableInit {
    Mutable(MutableInit),
    Const(ConstantInit),
}

impl Parse for VariableInit {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum starts] => Ok(VariableInit::Mutable(stream.parse()?)),
            tokens::Tok![enum contains] => Ok(VariableInit::Const(stream.parse()?)),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "variable initializer".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Init {
    Variable(VariableInit),
    Function(BlockList),
}

impl Init {
    pub fn as_var(&self) -> &VariableInit {
        match self {
            Init::Variable(var) => var,
            _ => panic!("Expected variable!"),
        }
    }

    pub fn as_fn(&self) -> &BlockList {
        match self {
            Init::Function(func) => func,
            _ => panic!("Expected fucntion!"),
        }
    }
}

impl Parse for Init {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum starts] | tokens::Tok![enum contains] => {
                Ok(Init::Variable(stream.parse()?))
            }
            _ => Ok(Init::Function(stream.parse()?)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    ident: tokens::Ident,
    ty: typess::OfType,
}

impl Parse for Argument {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Argument {
            ident: stream.parse()?,
            ty: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Arguments {
    with_tok: tokens::With,
    args_tok: tokens::Arguments,
    args: Punctuation<Argument, tokens::Comma>,
}

impl Parse for Arguments {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Arguments {
            with_tok: stream.parse()?,
            args_tok: stream.parse()?,
            args: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub def_tok: tokens::Define,
    pub what: Indefinite,
    pub called_tok: tokens::Called,
    pub ident: tokens::Ident,
    pub args: Option<Arguments>,
    pub that_tok: tokens::That,
    pub init: Init,
}

impl Parse for Definition {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let def_tok = stream.parse()?;
        let what: Indefinite = stream.parse()?;
        let called_tok = stream.parse()?;
        let ident = stream.parse()?;

        let args = if what.noun.is_fn() && matches!(stream.peek()?, Token::With(_)) {
            Some(stream.parse()?)  
        } else {
            None
        };
    
        Ok(Definition {
            def_tok,
            what,
            called_tok,
            ident,
            args,
            that_tok: stream.parse()?,
            init: stream.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct DefinitionAction {
    pub def_tok: tokens::Defines,
    pub what: Indefinite,
    pub called_tok: tokens::Called,
    pub ident: tokens::Ident,
    pub args: Option<Arguments>,
    pub that_tok: tokens::That,
    pub init: Init,
}

impl Parse for DefinitionAction {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        let def_tok = stream.parse()?;
        let what: Indefinite = stream.parse()?;

        Ok(DefinitionAction {
            called_tok: stream.parse()?,
            ident: stream.parse()?,
            args: match what.noun.is_fn().then(|| stream.parse()) {
                Some(s) => s?,
                None => None,
            },
            that_tok: stream.parse()?,
            init: stream.parse()?,
            def_tok,
            what,
        })
    }
}

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Definition(Definition),
}

impl Parse for TopLevelItem {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        match stream.peek()? {
            tokens::Tok![enum define] => stream.parse().map(TopLevelItem::Definition),
            tok => Err(ParseError::UnexpectedToken {
                found: tok.clone(),
                expected: "top level item".into(),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<TopLevelItem>,
}

impl Parse for Program {
    fn parse(stream: &'_ crate::ParseStream<'_>) -> crate::Result<Self> {
        Ok(Program {
            items: stream.parse()?,
        })
    }
}
