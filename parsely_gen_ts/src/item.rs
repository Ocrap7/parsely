use std::fmt::Write;

use parsely_parser::item::Item;

use crate::{module::Module, Result};

impl Module {
    pub fn gen_item(&self, buffer: &mut impl Write, _item: &Item) -> Result<()> {
        writeln!(buffer, "hello")?;
        Ok(())
    }
}
