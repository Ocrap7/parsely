use node_id::NodeId;
use parsely_lexer::Span;

mod node_id;

pub struct Item {
    pub id: NodeId,
    pub span: Span,
    pub kind: ItemKind,
}

pub enum ItemKind {}

pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ExprKind,
}

pub enum ExprKind {}

pub struct Lit {}

pub enum LitKind {
    Bool,
}
