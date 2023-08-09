use std::collections::HashMap;

use parsely_parser::types::Type;

use crate::types::GenType;

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { scopes: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            symbols: HashMap::new(),
        })
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    pub fn insert(&mut self, name: &str, ty: GenType) -> bool {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.symbols.contains_key(name) {
                return false;
            }

            scope.symbols.insert(name.to_string(), ty);
            return true;
        } else {
            return false;
        }
    }

    pub fn insert_new(&mut self, name: &str, ty: GenType) -> Option<String> {
        if let Some(scope) = self.scopes.last_mut() {
            let mut fname = name.to_string();
            let mut i = 0;

            while scope.symbols.contains_key(&fname) {
                fname = format!("{name}{i}");
                i += 1;
            }

            scope.symbols.insert(fname.clone(), ty);
            Some(fname)
        } else {
            None
        }
    }

    pub fn get_temp(&mut self, name: &str) -> Option<String> {
        if let Some(scope) = self.scopes.last_mut() {
            let mut fname = name.to_string();
            let mut i = 0;

            while scope.symbols.contains_key(&fname) {
                fname = format!("{name}{i}");
                i += 1;
            }

            Some(fname)
        } else {
            None
        }
    }

    pub fn find_symbol(&self, name: &str) -> Option<&GenType> {
        self.iter().rev().find_map(|map| map.get(name))
    }

    pub fn find_symbol_in_current(&self, name: &str) -> Option<&GenType> {
        self.scopes
            .last()
            .map(|scp| scp.symbols.get(name))
            .flatten()
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &HashMap<String, GenType>> {
        self.scopes.iter().map(|s| &s.symbols)
    }

    pub fn iter_all(&self) -> impl Iterator<Item = (&String, &GenType)> {
        self.iter().rev().flat_map(|map| map.iter())
    }
}

#[derive(Debug)]
pub struct Scope {
    symbols: HashMap<String, GenType>,
}
