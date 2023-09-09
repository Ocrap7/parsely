use std::{
    collections::HashMap,
    sync::{atomic::AtomicUsize, Arc},
};

use parsely_lexer::{tokens::Ident, Range};
use parsely_parser::{item::Binding, program::Program};

use crate::{pack::Pack, value::SymbolValue};
use parsely_diagnostics::{Diagnostic, Result};

pub struct Symbol {
    pub value: SymbolValue,
    pub usages: AtomicUsize,
    pub exported: bool,

    pub node: Binding,
}

pub struct Module {
    pub buffer: String,
    pub pack: Arc<Pack>,

    pub(crate) name: String,
    diagnostics: Vec<Diagnostic>,
    pub(crate) dirty: bool,

    old_unused: Vec<Symbol>,
    symbols: HashMap<String, Symbol>,
}

impl Module {
    const BUFFER_SIZE_INIT: usize = 1024;

    pub fn run_new(
        name: impl ToString,
        program: &Program,
        pack: Arc<Pack>,
        diagnostics: Vec<Diagnostic>,
    ) -> Result<Module> {
        let mut module = Module {
            buffer: String::new(),
            pack,

            name: name.to_string(),
            diagnostics,
            dirty: false,

            old_unused: Vec::new(),
            symbols: HashMap::new(),
        };

        match module.run(program) {
            Ok(_) | Err(Diagnostic::Caught(_)) => Ok(module),
            Err(e) => return Err(e),
        }
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub(crate) fn push_diagnostic(&mut self, error: Diagnostic) {
        self.diagnostics.push(error);
    }

    pub(crate) fn insert_symbol(
        &mut self,
        name: &str,
        value: impl Into<SymbolValue>,
        node: Binding,
    ) {
        let existing = self.symbols.insert(
            name.to_string(),
            Symbol {
                value: value.into(),
                usages: AtomicUsize::new(0),
                exported: false,
                node,
            },
        );

        if let Some(old) = existing {
            if old.usages.load(std::sync::atomic::Ordering::SeqCst) == 0 {
                self.old_unused.push(old)
            }
        }
    }

    pub fn find_symbol(&self, ident: &Ident) -> Result<&Symbol> {
        if let Some(sym) = self.symbols.get(&ident.value) {
            sym.usages.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

            Ok(sym)
        } else {
            Err(Diagnostic::SymbolNotFound(ident.clone()))
        }
    }

    pub fn iter_symbols(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.symbols.iter()
    }

    pub fn run(&mut self, program: &Program) -> Result<()> {
        let mut buffer = String::with_capacity(Self::BUFFER_SIZE_INIT);

        for item in &program.items {
            if let Err(diag) = self.gen_item(&mut buffer, item) {
                self.push_diagnostic(diag);
            }
        }

        self.buffer = buffer;
        self.dirty = !self.diagnostics.is_empty();
        Ok(())
    }
}
