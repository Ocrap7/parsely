use std::fmt::Write;

use parsely_parser::item::{ExternalFunction, Function, Parameter, TopLevelItem};

use crate::{
    module::{Buffers, Module},
    Result,
};

impl Module {
    pub(crate) fn gen_item<HB: Write, CB: Write>(
        &mut self,
        buffers: &mut Buffers<'_, '_, HB, CB>,
        item: &TopLevelItem,
    ) -> Result<()> {
        match item {
            TopLevelItem::Struct(item) => {
                let packed = if item.packed.is_some() {
                    "__attribute__((__packed__)) "
                } else {
                    ""
                };

                if item.export.is_some() && item.opaque.is_some() {
                    writeln!(buffers.header, "typedef struct {0} {0}; ", item.ident.value)?;

                    writeln!(
                        buffers.code,
                        "typedef struct {}{} {{ ",
                        packed, item.ident.value
                    )?;
                    self.gen_struct_body(buffers.code, item.body.value.iter())?;
                    writeln!(buffers.code, "}} {};\n", item.ident.value)?;
                } else if item.export.is_some() {
                    writeln!(
                        buffers.header,
                        "typedef struct {}{} {{ ",
                        packed, item.ident.value
                    )?;
                    self.gen_struct_body(buffers.header, item.body.value.iter())?;
                    writeln!(buffers.header, "}} {};\n", item.ident.value)?;
                } else {
                    writeln!(
                        buffers.code,
                        "typedef struct {}{} {{ ",
                        packed, item.ident.value
                    )?;
                    self.gen_struct_body(buffers.code, item.body.value.iter())?;
                    writeln!(buffers.code, "}} {};\n", item.ident.value)?;
                }
            }
            TopLevelItem::Function(func) => {
                self.symbol_table.push_scope();

                if func.export.is_some() {
                    self.gen_function_signature(buffers.header, func)?;
                    write!(buffers.header, ";")?;
                }

                self.gen_function_signature(buffers.code, func)?;

                if func.body.value.len() == 0 {
                    write!(buffers.code, " {{")?;
                } else {
                    writeln!(buffers.code, " {{")?;
                }

                for item in func.body.value.iter() {
                    self.gen_statement(buffers.code, item)?;
                }
                writeln!(buffers.code, "}}")?;

                let scp = self.symbol_table.pop_scope();
                println!("{:#?}", scp);
            }
            TopLevelItem::ExternalFunction(func) => {
                if func.export.is_some() {
                    self.gen_external_function_signature(buffers.header, func)?;
                } else {
                    self.gen_external_function_signature(buffers.code, func)?;
                }
            }
        }

        Ok(())
    }

    fn gen_struct_body<'p, B: Write>(
        &mut self,
        buffer: &mut B,
        body: impl Iterator<Item = &'p Parameter>,
    ) -> Result<()> {
        for item in body {
            self.gen_type(buffer, &item.parameter_type)?;
            writeln!(buffer, " {};", item.ident.value)?;
        }

        Ok(())
    }

    fn gen_function_signature<'p, B: Write>(
        &mut self,
        buffer: &mut B,
        func: &Function,
    ) -> Result<()> {
        self.gen_type(buffer, &func.return_type)?;
        write!(buffer, " {}", func.ident.value)?;

        write!(buffer, "(")?;
        self.gen_function_params(buffer, func.params.value.iter())?;
        write!(buffer, ")")?;
        Ok(())
    }

    fn gen_external_function_signature<'p, B: Write>(
        &mut self,
        buffer: &mut B,
        func: &ExternalFunction,
    ) -> Result<()> {
        write!(buffer, "extern ")?;
        self.gen_type(buffer, &func.return_type)?;
        write!(buffer, " {}", func.ident.value)?;

        write!(buffer, "(")?;
        self.gen_function_params(buffer, func.params.value.iter())?;
        write!(buffer, ");")?;
        Ok(())
    }

    fn gen_function_params<'p, B: Write>(
        &mut self,
        buffer: &mut B,
        mut params: impl Iterator<Item = &'p Parameter>,
    ) -> Result<()> {
        if let Some(item) = params.next() {
            self.gen_type(buffer, &item.parameter_type)?;
            writeln!(buffer, "{}", item.ident.value)?;
        }

        for item in params {
            self.gen_type(buffer, &item.parameter_type)?;
            writeln!(buffer, ", {}", item.ident.value)?;
        }

        Ok(())
    }
}
