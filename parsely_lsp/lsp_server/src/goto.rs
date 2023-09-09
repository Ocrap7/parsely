use parsely_gen_asm::module::Module;
use parsely_lexer::{AsSpan, Position, SearchPosition, Span};
use parsely_parser::{
    expression::Expression,
    item::{Binding, BoundValue, Item, Op},
    program::Program,
};

pub struct GotoDefinition<'a> {
    module: &'a Module,
}

impl<'a> GotoDefinition<'a> {
    pub fn new(module: &'a Module) -> GotoDefinition<'a> {
        GotoDefinition { module }
    }
}

impl GotoDefinition<'_> {
    pub fn run(&self, program: &Program, position: &Position) -> Option<Span> {
        program
            .as_span()
            .contains_position(position)
            .then_some(())?;

        let item_index = program.items.search_position(position)?;

        self.run_item(&program.items[item_index], position)
    }

    pub fn run_item(&self, item: &Item, position: &Position) -> Option<Span> {
        match item {
            Item::Binding(bind) => self.run_binding(bind, position),
            Item::Attribute(_) => None,
            Item::Poison => None,
        }
    }

    pub fn run_binding(&self, binding: &Binding, position: &Position) -> Option<Span> {
        match &binding.value {
            BoundValue::OpList(ops) => {
                if let Some(res) = ops.list.iter().find_map(|f| self.run_op(f, position)) {
                    return Some(res);
                }
            }
            _ => (),
        }

        None
    }

    pub fn run_op(&self, op: &Op, position: &Position) -> Option<Span> {
        self.run_expr(&op.args, position)
    }

    fn run_expr(&self, expr: &Expression, position: &Position) -> Option<Span> {
        match expr {
            Expression::BinOp(op) => {
                if op.left.as_span().contains_position(position) {
                    return self.run_expr(&op.left, position);
                }

                if op.right.as_span().contains_position(position) {
                    return self.run_expr(&op.right, position);
                }
            }
            Expression::Parens(p) => return self.run_expr(&p.value, position),
            Expression::Ident(ident) => {
                if let Ok(sym) = self.module.find_symbol(ident) {
                    return Some(sym.node.as_span());
                }
            }
            _ => (),
        }

        None
    }
}
