use std::fmt::Write;

use parsely_lexer::AsSpan;
use parsely_parser::item::{Binding, BoundValue, Item, Op};

use crate::{module::Module, value::SymbolValue};

use parsely_diagnostics::{Diagnostic, DiagnosticLevel, Result};

impl Module {
    pub fn gen_item(&mut self, buffer: &mut impl Write, item: &Item) -> Result<()> {
        match item {
            Item::Binding(binding) => self.gen_binding(buffer, binding)?,
            Item::Attribute(attr) => {
                write!(buffer, ";; Attr")?;
                self.gen_expression(buffer, &attr.value.value)?;
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn gen_binding(&mut self, buffer: &mut impl Write, binding: &Binding) -> Result<()> {
        match &binding.value {
            BoundValue::OpList(ops) => {
                writeln!(buffer, "{}:", binding.ident.value)?;

                for op in ops.list.iter() {
                    if let Err(diag) = self.gen_op(buffer, op) {
                        self.push_diagnostic(diag);
                    }
                }

                self.insert_symbol(&binding.ident.value, SymbolValue::Function(), binding.clone());
            }
            BoundValue::Expression(expr) => {
                writeln!(buffer, ".set {}, ", binding.ident.value)?;
                let value = self.gen_expression(buffer, expr)?;

                self.insert_symbol(&binding.ident.value, value, binding.clone());
            }
        }
        Ok(())
    }

    fn gen_op(&mut self, buffer: &mut impl Write, op: &Op) -> Result<()> {
        if let Some(instr) = self.pack.instructions.get(&op.op.value) {
            match instr.transform(&self.pack, &op.op.value, &op.args) {
                Ok(val) => write!(buffer, "    {}", val)?,
                Err(d) => self.push_diagnostic(d),
            }
        } else {
            return Err(Diagnostic::Message(
                format!(
                    "Instruction `{}` was not found in the current pack",
                    op.op.value
                ),
                op.op.as_span(),
                DiagnosticLevel::Error,
            ));
        }

        Ok(())
    }
}
