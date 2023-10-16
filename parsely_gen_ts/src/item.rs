use std::fmt::Write;

use parsely_parser::item::{Argument, Arguments, ElementBody, Inputs, Item};

use crate::{module::Module, Result};

impl Module {
    pub fn gen_item(&self, buffer: &mut impl Write, item: &Item) -> Result<Option<Inputs>> {
        match item {
            Item::Expression(expr) => self.gen_expression(buffer, expr)?,
            Item::Element(element) => {
                write!(buffer, "<{}", element.tag.value)?;
                if let Some(args) = &element.args {
                    write!(buffer, " ")?;
                    self.gen_arguments(buffer, args)?;
                }
                write!(buffer, ">")?;

                if !self.gen_body(buffer, &element.body)? {
                    write!(buffer, "</{}>", element.tag.value)?;
                }
            }
            Item::Inputs(i) => return Ok(Some(i.clone())),
        }
        Ok(None)
    }

    fn gen_arguments(&self, buffer: &mut impl Write, args: &Arguments) -> Result<()> {
        let mut write_arg = |arg: &Argument| -> Result<()> {
            write!(buffer, "{}", arg.key)?;

            if let Some((_, expr)) = &arg.value {
                write!(buffer, "=\"")?;
                self.gen_expression(buffer, expr)?;
                write!(buffer, "\"")?;
            }

            Ok(())
        };

        for arg in args.0.value.iter() {
            write_arg(arg)?;
        }

        Ok(())
    }

    /// Generates an element body
    ///
    /// Returns Ok(true) if element has no body
    fn gen_body(&self, buffer: &mut impl Write, body: &ElementBody) -> Result<bool> {
        match body {
            ElementBody::Child(child) => {
                if let Some(body) = &child.child {
                    self.gen_expression(buffer, body)?;
                } else {
                    return Ok(true);
                }
            }
            ElementBody::Children(body) => {
                for child in body.value.iter() {
                    self.gen_item(buffer, child)?;
                }
            }
        }

        Ok(false)
    }
}
