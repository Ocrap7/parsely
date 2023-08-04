use std::collections::HashMap;

use crate::{
    llvm_value::{Function, Type, Variable},
    GenError, Result,
};

#[derive(Debug)]
pub struct SymbolTable<'ctx> {
    scopes: Vec<Scope<'ctx>>,
}

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> SymbolTable<'ctx> {
        SymbolTable { scopes: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
        })
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    /* Variables */

    pub fn insert_variable(&mut self, name: &str, ty: Variable<'ctx>) -> bool {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.variables.contains_key(name) {
                return false;
            }

            scope.variables.insert(name.to_string(), ty);
            true
        } else {
            false
        }
    }

    pub fn find_variable(&self, name: &str) -> Result<&Variable<'ctx>> {
        self.iter_variable()
            .rev()
            .find_map(|map| map.get(name))
            .ok_or_else(|| GenError::SymbolNotFound(name.to_string()))
    }

    pub fn find_variable_in_current(&self, name: &str) -> Option<&Variable<'ctx>> {
        self.scopes
            .last()
            .and_then(|scp| scp.variables.get(name))
    }

    pub fn iter_variable(
        &self,
    ) -> impl DoubleEndedIterator<Item = &HashMap<String, Variable<'ctx>>> {
        self.scopes.iter().map(|s| &s.variables)
    }

    pub fn iter_all_variables(&self) -> impl Iterator<Item = (&String, &Variable<'ctx>)> {
        self.iter_variable().rev().flat_map(|map| map.iter())
    }

    /* Functions */

    pub fn insert_function(&mut self, name: &str, ty: Function<'ctx>) -> bool {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.functions.contains_key(name) {
                return false;
            }

            scope.functions.insert(name.to_string(), ty);
            true
        } else {
            false
        }
    }

    pub fn find_function(&self, name: &str) -> Result<&Function<'ctx>> {
        self.iter_functions()
            .rev()
            .find_map(|map| map.get(name))
            .ok_or_else(|| GenError::SymbolNotFound(name.to_string()))
    }

    pub fn find_function_in_current(&self, name: &str) -> Option<&Function<'ctx>> {
        self.scopes
            .last()
            .and_then(|scp| scp.functions.get(name))
    }

    pub fn iter_functions(
        &self,
    ) -> impl DoubleEndedIterator<Item = &HashMap<String, Function<'ctx>>> {
        self.scopes.iter().map(|s| &s.functions)
    }

    pub fn iter_all_functions(&self) -> impl Iterator<Item = (&String, &Function<'ctx>)> {
        self.iter_functions().rev().flat_map(|map| map.iter())
    }

    /* Types */

    pub fn insert_type(&mut self, name: &str, ty: Type<'ctx>) -> bool {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.types.contains_key(name) {
                return false;
            }

            scope.types.insert(name.to_string(), ty);
            true
        } else {
            false
        }
    }

    pub fn find_type(&self, name: &str) -> Option<&Type<'ctx>> {
        self.iter_types().rev().find_map(|map| map.get(name))
    }

    pub fn find_type_in_current(&self, name: &str) -> Option<&Type<'ctx>> {
        self.scopes.last().and_then(|scp| scp.types.get(name))
    }

    pub fn iter_types(&self) -> impl DoubleEndedIterator<Item = &HashMap<String, Type<'ctx>>> {
        self.scopes.iter().map(|s| &s.types)
    }

    pub fn iter_all_types(&self) -> impl Iterator<Item = (&String, &Type<'ctx>)> {
        self.iter_types().rev().flat_map(|map| map.iter())
    }
}

#[derive(Debug)]
pub struct Scope<'ctx> {
    variables: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, Function<'ctx>>,
    types: HashMap<String, Type<'ctx>>,
}
