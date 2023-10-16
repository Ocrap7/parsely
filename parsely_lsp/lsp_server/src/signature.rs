use parsely_gen_asm::module::Module;
use parsely_lexer::{AsSpan, Position, SearchPosition};
use parsely_parser::{
    expression::Expression,
    item::{Binding, BoundValue, Item, Op},
    program::Program,
};
use tower_lsp::lsp_types::{ParameterInformation, ParameterLabel, SignatureInformation};

pub struct SignatureProvider<'a> {
    module: &'a Module,
}

impl<'a> SignatureProvider<'a> {
    pub fn new(module: &'a Module) -> SignatureProvider<'a> {
        SignatureProvider { module }
    }
}

impl SignatureProvider<'_> {
    pub fn run(
        &self,
        program: &Program,
        position: &Position,
    ) -> Option<Option<SignatureInformation>> {
        program
            .as_span()
            .contains_position(position)
            .then_some(())?;

        let item_index = program.items.search_position(position)?;

        self.run_item(&program.items[item_index], position)
    }

    pub fn run_item(
        &self,
        item: &Item,
        position: &Position,
    ) -> Option<Option<SignatureInformation>> {
        match item {
            Item::Binding(bind) => self.run_binding(bind, position),
            Item::Attribute(_) => None,
            Item::Poison => None,
        }
    }

    pub fn run_binding(
        &self,
        binding: &Binding,
        position: &Position,
    ) -> Option<Option<SignatureInformation>> {
        if binding.value.as_span().contains_position(position) {
            match &binding.value {
                BoundValue::OpList(ops) => {
                    if let Some(res) = ops.list.iter().find_map(|f| self.run_op(f, position)) {
                        return Some(res);
                    }
                }
                _ => (),
            }
        }

        None
    }

    pub fn run_op(&self, op: &Op, position: &Position) -> Option<Option<SignatureInformation>> {
        if let Some(instr) = self.module.pack.instructions.get(&op.op.value) {
            let mut str = op.op.value.clone();

            let parameters: Vec<_> = instr
                .input
                .iter()
                .map(|param| {
                    let param_str = format!(" {}", param);

                    let info = ParameterInformation {
                        label: ParameterLabel::LabelOffsets([
                            str.len() as u32,
                            str.len() as u32 + param_str.len() as u32,
                        ]),
                        documentation: None,
                    };

                    str.push_str(&param_str);

                    info
                })
                .collect();

            let mut len = 0;
            self.expr_len(&op.args, &mut len);

            let index = self.expr_active_index(&op.args, position, 0).unwrap_or(len);

            return Some(Some(SignatureInformation {
                label: str,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: Some(index as u32),
            }));
        }

        Some(None)
    }

    /// Returns (in len param) the number of terminal nodes in the expression
    fn expr_len(&self, expr: &Expression, len: &mut usize) {
        match expr {
            Expression::BinOp(op) => {
                self.expr_len(&op.left, len);

                *len += 1; // We count the operator

                self.expr_len(&op.right, len);
            }
            _ => {
                if !expr.as_span().is_dummy() {
                    *len += 1
                }
            }
        }
    }

    /// Returns the linear index of the terminal node under the cursor
    fn expr_active_index(
        &self,
        expr: &Expression,
        position: &Position,
        index: usize,
    ) -> Option<usize> {
        match expr {
            Expression::BinOp(op) => {
                if let Some(index) = self.expr_active_index(&op.left, position, index) {
                    return Some(index);
                }

                if op.op.as_span().contains_position(position) {
                    return Some(index + 1);
                }

                self.expr_active_index(&op.right, position, index + 2)
            }
            _ => {
                if expr.as_span().contains_position(position) {
                    Some(index)
                } else {
                    None
                }
            }
        }
    }
}
