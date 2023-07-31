use std::{fmt::Write, io};

use parsely_parser::item::{Program, TopLevelItem};

use crate::{symbols::SymbolTable, Result};

pub(crate) struct Buffers<'hdr, 'code, HB: Write, CB: Write> {
    pub(crate) header: &'hdr mut HB,
    pub(crate) code: &'code mut CB,
}

pub struct Module {
    pub(crate) name: String,
    pub(crate) symbol_table: SymbolTable,
}

impl Module {
    pub fn new(name: impl ToString) -> Module {
        Module {
            name: name.to_string(),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn run(mut self, program: &Program) -> Result<(String, String)> {
        let mut header = String::with_capacity(256);
        let mut code = String::with_capacity(256);

        let mut buffers = Buffers {
            header: &mut header,
            code: &mut code,
        };

        write!(
            buffers.header,
            "#ifndef {}\n#define {}\n",
            self.name, self.name
        )?;

        writeln!(buffers.header, "#include \"../lib/core.h\"")?;
        writeln!(buffers.code, "#include \"{}.h\"", self.name)?;

        for item in program.items.iter() {
            self.gen_item(&mut buffers, item)?;
        }

        write!(buffers.header, "\n#endif")?;

        Ok((header, code))
    }
}
