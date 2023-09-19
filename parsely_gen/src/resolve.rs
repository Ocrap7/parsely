use std::{collections::HashMap, sync::Arc};

use parsely_lexer::Tok;
// use parsely_lexer::tokens::Type;
use parsely_parser::{
    expression::{Expression, ExpressionKind, Literal},
    item::{Item, PatternKind, TypeBinding, UnionVarientKind},
    types::{self, ArraySize},
    // types::TypeKind,
    NodeId,
};

use crate::{
    path::{Path, PathSegment},
    ty::{Type, TypeKind},
    visitor::Visitor,
};

#[derive(Debug)]
pub struct Symbol {
    pub node_id: Option<NodeId>,
    pub name: PathSegment,
    pub kind: SymbolKind,
    pub export: bool,
    pub children: SymbolTable,
}

#[derive(Debug)]
pub enum SymbolKind {
    Module,
    Function,
    Variable,
    Type,
}

#[derive(Debug)]
pub struct SymbolTable {
    name: PathSegment,
    symbols: HashMap<PathSegment, Symbol>,
}

impl SymbolTable {
    pub fn new(name: &PathSegment) -> SymbolTable {
        SymbolTable {
            name: name.clone(),
            symbols: HashMap::new(),
        }
    }

    pub fn resolve(&self, path: &[PathSegment]) -> bool {
        let mut iter = path.iter();
        self.resolve_impl(&mut iter)
    }

    pub fn resolve_impl<'a>(&self, path: &mut impl Iterator<Item = &'a PathSegment>) -> bool {
        if let Some(seg) = path.next() {
            let Some(sym) = self.symbols.get(seg) else {
                return false;
            };

            sym.children.resolve_impl(path)
            // match &sym.kind {
            //     SymbolKind::Module(m) => m.resolve_impl(path),
            //     SymbolKind::Function(m) => m.resolve_impl(path),
            //     _ => false,
            // }
        } else {
            true
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    root_table: SymbolTable,
    current_path: Vec<PathSegment>,

    imports: Vec<Path>,
}

impl Environment {
    pub fn new(root_name: &str) -> Environment {
        let root = PathSegment::new(root_name);

        let mut current_path = Vec::with_capacity(16);
        current_path.push(root.clone());

        let mut symbols = HashMap::new();
        symbols.insert(
            root.clone(),
            Symbol {
                kind: SymbolKind::Module,
                name: root.clone(),
                node_id: None,
                export: false,
                children: SymbolTable::new(&root),
            },
        );

        Environment {
            root_table: SymbolTable {
                name: root,
                symbols,
            },
            current_path,
            imports: Vec::new(),
        }
    }

    pub fn push_segment(&mut self, segment: &PathSegment) {
        self.current_path.push(segment.clone());
    }

    pub fn pop_segment(&mut self) {
        self.current_path.pop();
    }

    pub fn follow_absolute(&self, path: &Path) -> Option<&Symbol> {
        let mut iter = path.iter();

        Environment::follow_absolute_impl(&self.root_table, &mut iter).flatten()
    }

    fn follow_absolute_impl<'a, 'p>(
        sym: &'a SymbolTable,
        path: &mut impl Iterator<Item = &'p PathSegment>,
    ) -> Option<Option<&'a Symbol>> {
        if let Some(segment) = path.next() {
            let sym = sym.symbols.get(segment)?;
            println!("{:?}", sym.name);

            match Environment::follow_absolute_impl(&sym.children, path) {
                Some(Some(a)) => Some(Some(a)),
                Some(None) => Some(Some(sym)),
                None => None,
            }
        } else {
            Some(None)
        }
    }

    pub fn resolve_path(&self, path: &Path) -> Option<Path> {
        let mut iter = self.current_path.iter();
        let mut new_path = Vec::new();

        Environment::resolve_path_impl(&self.root_table, &mut iter, path.as_ref(), &mut new_path)?;

        Some(Path::from(new_path.into_iter().rev()))
    }

    pub fn resolve_path_impl<'a>(
        sym: &'a SymbolTable,
        ind: &mut impl Iterator<Item = &'a PathSegment>,
        path: &[PathSegment],
        new_path: &mut Vec<PathSegment>,
    ) -> Option<()> {
        if let Some(index) = ind.next() {
            let symbol = sym.symbols.get(index)?;

            if let Some(_) = Environment::resolve_path_impl(&symbol.children, ind, path, new_path) {
                new_path.push(symbol.children.name.clone());

                return Some(());
            }
        }

        if sym.resolve(path) {
            new_path.extend(path.iter().rev().cloned());
            Some(())
        } else {
            None
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let mut iter = self.current_path.iter();

        Environment::lookup_impl(&self.root_table, &mut iter, name)
    }

    fn lookup_impl<'a>(
        sym: &'a SymbolTable,
        ind: &mut impl Iterator<Item = &'a PathSegment>,
        name: &str,
    ) -> Option<&'a Symbol> {
        if let Some(index) = ind.next() {
            let sym = sym.symbols.get(index)?;
            // let sym_table = sym.table()?;

            Environment::lookup_impl(&sym.children, ind, name).or_else(|| {
                if index == name {
                    Some(sym)
                } else {
                    None
                }
            })
        } else {
            sym.symbols.get(name)
        }
    }

    pub fn current_mut(&mut self) -> Option<&mut SymbolTable> {
        let mut iter = self.current_path.iter();

        Environment::current_mut_impl(&mut self.root_table, &mut iter)
    }

    fn current_mut_impl<'a>(
        sym: &'a mut SymbolTable,
        ind: &mut impl Iterator<Item = &'a PathSegment>,
    ) -> Option<&'a mut SymbolTable> {
        if let Some(index) = ind.next() {
            let sym = sym.symbols.get_mut(index)?;
            // let sym_table = sym.table_mut()?;

            Environment::current_mut_impl(&mut sym.children, ind)
        } else {
            Some(sym)
        }
    }

    // pub fn declare()

    // pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
    //     let mut iter = self.current_path.iter();

    //     Environment::lookup_impl_mut(&mut self.root_table, &mut iter, name)
    // }

    // fn lookup_impl_mut<'a>(
    //     sym: &'a mut SymbolTable,
    //     ind: &mut impl Iterator<Item = &'a PathSegment>,
    //     name: &str,
    // ) -> Option<&'a mut Symbol> {
    //     if let Some(index) = ind.next() {
    //         let sym = sym.symbols.get_mut(index)?;
    //         let sym_table = sym.table_mut()?;

    //         if let Some(sym) = Environment::lookup_impl_mut(sym_table, ind, name) {
    //             return Some(sym);
    //         }

    //         if index == name {
    //             Some(sym)
    //         } else {
    //             None
    //         }
    //     } else {
    //         sym.symbols.get_mut(name)
    //     }
    // }
}

pub trait Resolver {
    fn env(&mut self) -> &mut Environment;
    fn node_to_path(&mut self) -> &mut HashMap<NodeId, Path>;
    fn path_to_node(&mut self) -> &mut HashMap<Path, NodeId>;

    fn declare_fn(&mut self, name: &str, id: &NodeId, export: bool) -> Path {
        let seg = self.declare(name, id, SymbolKind::Function, export);
        self.env().current_path.push(seg.clone());

        self.env().current_path.iter().cloned().into()
    }

    fn declare_mod(&mut self, name: &str, id: &NodeId, export: bool) -> Path {
        let seg = self.declare(name, id, SymbolKind::Module, export);
        self.env().current_path.push(seg.clone());

        self.env().current_path.iter().cloned().into()
    }

    fn declare_var(&mut self, name: &str, id: &NodeId, export: bool) -> Path {
        let segment = self.declare(name, id, SymbolKind::Variable, export);

        self.env()
            .current_path
            .iter()
            .chain([&segment].into_iter())
            .cloned()
            .into()
    }

    fn declare_type(&mut self, name: &str, id: &NodeId, export: bool) -> Path {
        let segment = self.declare(name, id, SymbolKind::Type, export);

        self.env()
            .current_path
            .iter()
            .chain([&segment].into_iter())
            .cloned()
            .into()
    }

    fn declare(
        &mut self,
        name: &str,
        id: &NodeId,
        symbol_kind: SymbolKind,
        export: bool,
    ) -> PathSegment {
        let current = self
            .env()
            .current_mut()
            .expect("Unable to get current environment!");

        let seg = PathSegment::new(name);
        let symbol = Symbol {
            kind: symbol_kind,
            name: seg.clone(),
            node_id: Some(id.clone()),
            export,
            children: SymbolTable::new(&seg),
        };

        current.symbols.insert(seg.clone(), symbol);

        seg
    }

    fn pop_scope(&mut self) {
        self.env().current_path.pop();
    }
}

#[derive(Debug)]
pub struct DeclarationResolver {
    env: Environment,

    node_to_path: HashMap<NodeId, Path>,
    path_to_node: HashMap<Path, NodeId>,
}

impl DeclarationResolver {
    pub fn new(package: &str) -> DeclarationResolver {
        DeclarationResolver {
            env: Environment::new(package),
            node_to_path: HashMap::new(),
            path_to_node: HashMap::new(),
            // resolved: HashMap::new(),
        }
    }

    pub fn to_interface_resolve(self) -> InterfaceResolver {
        InterfaceResolver {
            env: self.env,
            node_to_path: self.node_to_path,
            path_to_node: self.path_to_node,
            node_to_type: HashMap::new(),
        }
    }
}

impl Resolver for DeclarationResolver {
    fn env(&mut self) -> &mut Environment {
        &mut self.env
    }

    fn node_to_path(&mut self) -> &mut HashMap<NodeId, Path> {
        &mut self.node_to_path
    }

    fn path_to_node(&mut self) -> &mut HashMap<Path, NodeId> {
        &mut self.path_to_node
    }
}

impl Visitor for DeclarationResolver {
    fn default_value(&mut self) -> () {
        ()
    }

    fn visit_fn_decl(&mut self, function: &parsely_parser::item::FunctionBinding, item: &Item) {
        let path = self.declare_fn(&function.ident.value, &item.id, item.export_tok.is_some());

        for param in function.parameters.value.iter() {
            param.pattern.visit_idents(&mut |id, pat| {
                println!("pAttern : {:?}", pat.id);
                let path = self.declare_var(&id.value, &pat.id, item.export_tok.is_some());

                self.node_to_path.insert(pat.id.clone(), path.clone());
                self.path_to_node.insert(path, pat.id.clone());
            });
        }

        for item in &function.value {
            self.visit_item(item);
        }

        self.node_to_path.insert(item.id.clone(), path.clone());
        self.path_to_node.insert(path, item.id.clone());

        self.pop_scope();
    }

    fn visit_module_decl(&mut self, module: &parsely_parser::item::Module, item: &Item) {
        if let (Some(ident), Some(body)) = (&module.ident, &module.body) {
            let path = self.declare_mod(&ident.value, &item.id, item.export_tok.is_some());

            for item in &body.1 {
                self.visit_item(item);
            }

            self.node_to_path.insert(item.id.clone(), path.clone());
            self.path_to_node.insert(path, item.id.clone());

            self.pop_scope();
        }
    }

    fn visit_var_decl(&mut self, var: &parsely_parser::item::ValueBinding, item: &Item) {
        var.pattern.visit_idents(&mut |id, pat| {
            let path = self.declare_var(&id.value, &pat.id, item.export_tok.is_some());

            self.node_to_path.insert(pat.id.clone(), path.clone());
            self.path_to_node.insert(path, pat.id.clone());

            // self.pop_scope();
        });
    }

    fn visit_type_decl(&mut self, type_decl: &parsely_parser::item::TypeAlias, item: &Item) {
        let path = self.declare_type(&type_decl.ident.value, &item.id, item.export_tok.is_some());
        // println!("{:?}", self.env.current_path);

        self.node_to_path.insert(item.id.clone(), path.clone());
        self.path_to_node.insert(path, item.id.clone());

        // self.pop_scope();
    }
}

#[derive(Debug)]
pub struct InterfaceResolver {
    env: Environment,

    node_to_path: HashMap<NodeId, Path>,
    path_to_node: HashMap<Path, NodeId>,

    node_to_type: HashMap<NodeId, Type>,
}

impl InterfaceResolver {
    pub fn to_type_resolver(self) -> TypeResolver {
        TypeResolver {
            env: self.env,
            node_to_path: self.node_to_path,
            path_to_node: self.path_to_node,

            node_to_type: self.node_to_type,
            // node_to_value_type: HashMap::new(),
            type_hint: None,
        }
    }
}

impl Resolver for InterfaceResolver {
    fn env(&mut self) -> &mut Environment {
        &mut self.env
    }

    fn node_to_path(&mut self) -> &mut HashMap<NodeId, Path> {
        &mut self.node_to_path
    }

    fn path_to_node(&mut self) -> &mut HashMap<Path, NodeId> {
        &mut self.path_to_node
    }
}

fn convert_type(ty: &types::Type, resolver: &mut impl Resolver, map_path: bool) -> Type {
    match &ty.kind {
        types::TypeKind::Infer(_) => Type::new_infer(),
        types::TypeKind::Named(path) => {
            match (path.segments.len(), path.segments.iter().next()) {
                (1, Some(parsely_lexer::tokens::Ident { value, .. })) => {
                    if let Some("int") = value.get(0..3) {
                        return if let Some(size) = value.get(3..) {
                            let size = (size.len() > 0)
                                .then(|| size.parse().expect("TODO: error handling size"));

                            Type::new_int(size)
                        } else {
                            Type::new_int(None)
                        };
                    } else if let Some("uint") = value.get(0..4) {
                        return if let Some(size) = value.get(4..) {
                            let size = (size.len() > 0)
                                .then(|| size.parse().expect("TODO: error handling size"));

                            Type::new_uint(size)
                        } else {
                            Type::new_uint(None)
                        };
                    } else if let Some("float") = value.get(0..5) {
                        return if let Some(size) = value.get(5..) {
                            let size = (size.len() > 0)
                                .then(|| size.parse().expect("TODO: error handling size"));

                            Type::new_float(size)
                        } else {
                            Type::new_float(None)
                        };
                    } else if let Some("bool") = value.get(0..4) {
                        return Type {
                            kind: TypeKind::Bool,
                        };
                    } else if let Some("char") = value.get(0..4) {
                        return Type {
                            kind: TypeKind::Char,
                        };
                    } else if let Some("str") = value.get(0..3) {
                        return Type {
                            kind: TypeKind::Str,
                        };
                    }
                }
                _ => {}
            }

            let path = Path::from_path(path);
            let resolved = resolver.env().resolve_path(&path).unwrap();

            let id = resolver
                .path_to_node()
                .get(&resolved)
                .expect("Unable to get node from path!")
                .clone();

            if map_path {
                resolver.node_to_path().insert(ty.id.clone(), resolved);
            }

            Type {
                kind: TypeKind::Tbd { id },
            }
        }
        types::TypeKind::Array(arr) => {
            let size = match &arr.value.size {
                ArraySize::Infer(_) => None,
                ArraySize::Size(box Expression {
                    kind: ExpressionKind::Literal(Literal::Int(val)),
                    ..
                }) => Some(val.value.value as usize),
                _ => panic!(),
            };

            Type {
                kind: TypeKind::Array {
                    base: Box::new(convert_type(&arr.value.ty, resolver, map_path)),
                    size,
                },
            }
        }
        types::TypeKind::Slice(arr) => Type {
            kind: TypeKind::Slice {
                base: Box::new(convert_type(&arr.value.1, resolver, map_path)),
            },
        },
        types::TypeKind::IndexRef(ty, brk) => Type {
            kind: TypeKind::IndexRef {
                base: Box::new(convert_type(ty, resolver, map_path)),
                mutable: brk.value.mut_tok.is_some(),
            },
        },
        types::TypeKind::Optional(ty, _) => Type {
            kind: TypeKind::Optional {
                base: Box::new(convert_type(ty, resolver, map_path)),
            },
        },
        types::TypeKind::Ref(ty, rp) => Type {
            kind: TypeKind::Ref {
                base: Box::new(convert_type(ty, resolver, map_path)),
                mutable: rp.mut_tok.is_some(),
            },
        },
        types::TypeKind::Tuple(ty) => Type {
            kind: TypeKind::Tuple {
                fields: ty
                    .iter()
                    .map(|ty| convert_type(ty, resolver, map_path))
                    .collect(),
            },
        },
        _ => Type::default(),
    }
}

impl Visitor<Type> for InterfaceResolver {
    fn default_value(&mut self) -> Type {
        Type::default()
    }

    fn visit_type(&mut self, ty: &parsely_parser::types::Type) -> Type {
        convert_type(ty, self, true)
    }

    fn visit_fn_decl(&mut self, function: &parsely_parser::item::FunctionBinding, item: &Item) {
        let Some(path) = self.node_to_path.get(&item.id) else {
            return;
        };

        self.env.push_segment(path.last());


        for param in function.parameters.value.iter() {
            self.visit_type(&param.ty);
        }

        for item in &function.value {
            self.visit_item(item);
        }

        self.pop_scope();
    }

    fn visit_module_decl(&mut self, module: &parsely_parser::item::Module, item: &Item) {
        if let (Some(ident), Some(body)) = (&module.ident, &module.body) {
            let Some(path) = self.node_to_path.get(&item.id) else {
                return;
            };

            self.env.push_segment(path.last());

            for item in &body.1 {
                self.visit_item(item);
            }

            self.pop_scope();
        }
    }

    fn visit_var_decl(&mut self, var: &parsely_parser::item::ValueBinding, item: &Item) {
        var.pattern.visit_idents(&mut |id, pat| {
            let Some(path) = self.node_to_path.get(&pat.id) else {
                return;
            };

            self.env.push_segment(path.last());

            self.pop_scope();
        });

        if let Some((_, ty)) = &var.ty_annotation {
            self.visit_type(ty);
        }
    }

    fn visit_type_decl(&mut self, type_decl: &parsely_parser::item::TypeAlias, item: &Item) {
        let Some(path) = self.node_to_path.get(&item.id) else {
            return;
        };

        self.env.push_segment(path.last());

        match &type_decl.ty {
            TypeBinding::Type(ty) => {
                let ty = self.visit_type(ty);

                let kind = TypeKind::Alias {
                    id: item.id.clone(),
                    base: Box::new(ty),
                };

                self.node_to_type.insert(item.id.clone(), Type { kind });
            }
            TypeBinding::Named(named) => {
                let kind = TypeKind::Struct {
                    id: item.id.clone(),
                    fields: named
                        .iter()
                        .filter_map(|field| {
                            let PatternKind::Ident(ident) = &field.pattern.kind else {
                                        return None
                                    };

                            let ty = self.visit_type(&field.ty);
                            self.node_to_type.insert(field.id.clone(), ty.clone());

                            Some((Arc::from(ident.value.as_str()), ty))
                        })
                        .collect(),
                };

                self.node_to_type.insert(item.id.clone(), Type { kind });
            }
            TypeBinding::Union(union) => {
                let kind = TypeKind::Union {
                    id: item.id.clone(),
                    tag: union
                        .tag_type
                        .as_ref()
                        .map(|(tt, _)| Box::new(self.visit_type(tt))),
                    fields: union
                        .values
                        .iter()
                        .map(|field| {
                            let ty = match &field.kind {
                                UnionVarientKind::Named(named) => (
                                    Arc::from(named.value.as_str()),
                                    Type {
                                        kind: TypeKind::Unit,
                                    },
                                ),
                                UnionVarientKind::Typed(name, _, ty) => {
                                    (Arc::from(name.value.as_str()), self.visit_type(&ty))
                                }
                            };

                            self.node_to_type.insert(field.id.clone(), ty.1.clone());

                            ty
                        })
                        .collect(),
                };

                self.node_to_type.insert(item.id.clone(), Type { kind });
            }
            _ => unimplemented!(),
        }

        self.pop_scope();
    }
}

#[derive(Debug)]
pub struct TypeResolver {
    pub env: Environment,

    pub node_to_path: HashMap<NodeId, Path>,
    pub path_to_node: HashMap<Path, NodeId>,

    pub node_to_type: HashMap<NodeId, Type>,
    // pub node_to_value_type: HashMap<NodeId, Type>,
    type_hint: Option<Type>,
}

impl TypeResolver {
    fn with_type_hint<U>(&mut self, f: impl Fn(&mut TypeResolver) -> U, ty: Type) -> (U, Type) {
        let old_hint = self.type_hint.replace(ty);

        let val = f(self);

        let ty = self.type_hint.take().unwrap();
        self.type_hint = old_hint;

        (val, ty)
    }

    fn ttt(&self, ty: *mut Type) {
        match unsafe { &mut (*ty).kind } {
            TypeKind::Tbd { ref id } => {
                let to_ty = self.node_to_type.get(id).expect("Expected tbd type");

                unsafe {
                    *ty = to_ty.clone();
                }
            }
            TypeKind::Alias { base, .. } => self.ttt(base.as_mut()),
            TypeKind::IndexRef { base, .. } => self.ttt(base.as_mut()),
            TypeKind::Ref { base, .. } => self.ttt(base.as_mut()),
            TypeKind::Slice { base, .. } => self.ttt(base.as_mut()),
            TypeKind::Optional { base, .. } => self.ttt(base.as_mut()),
            TypeKind::Tuple { fields, .. } => fields.iter_mut().for_each(|field| self.ttt(field)),
            TypeKind::Struct { fields, .. } => {
                fields.values_mut().for_each(|field| self.ttt(field))
            }
            TypeKind::Union { fields, .. } => fields.values_mut().for_each(|field| self.ttt(field)),
            _ => (),
        }
    }
}

impl Resolver for TypeResolver {
    fn env(&mut self) -> &mut Environment {
        &mut self.env
    }

    fn node_to_path(&mut self) -> &mut HashMap<NodeId, Path> {
        &mut self.node_to_path
    }

    fn path_to_node(&mut self) -> &mut HashMap<Path, NodeId> {
        &mut self.path_to_node
    }
}

impl Visitor<Type> for TypeResolver {
    fn visit_type(&mut self, ty: &parsely_parser::types::Type) -> Type {
        convert_type(ty, self, false)
    }

    fn visit_type_decl(
        &mut self,
        type_decl: &parsely_parser::item::TypeAlias,
        item: &parsely_parser::item::Item,
    ) {
        let t: *mut Type = self
            .node_to_type
            .get_mut(&item.id)
            .expect("Unable to get type decl");

        self.ttt(t);
    }

    fn visit_module_decl(
        &mut self,
        module: &parsely_parser::item::Module,
        item: &parsely_parser::item::Item,
    ) {
        if let (Some(ident), Some(body)) = (&module.ident, &module.body) {
            let Some(path) = self.node_to_path.get(&item.id) else {
                return;
            };

            self.env.push_segment(path.last());

            for item in &body.1 {
                self.visit_item(item);
            }

            self.env.pop_segment();
        }
    }

    fn visit_fn_decl(
        &mut self,
        function: &parsely_parser::item::FunctionBinding,
        item: &parsely_parser::item::Item,
    ) {
        let params = function
            .parameters
            .value
            .iter()
            .map(|p| {
                let ty = self.visit_type(&p.ty);

                self.node_to_type.insert(p.pattern.id, ty.clone());

                let name = match &p.pattern.kind {
                    PatternKind::Ident(id) => Arc::from(id.value.as_str()),
                    _ => panic!(),
                };

                (name, ty)
            })
            .collect();

        let ret_ty = if let Some((_, ret_ty)) = &function.ret_type {
            self.visit_type(&ret_ty)
        } else {
            Type::new_unit()
        };

        let fty = Type {
            kind: TypeKind::Function {
                id: item.id.clone(),
                params,
                return_ty: Box::new(ret_ty),
            },
        };

        self.node_to_type.insert(item.id.clone(), fty);

        let Some(path) = self.node_to_path.get(&item.id) else {
            return;
        };

        self.env.push_segment(path.last());

        self.visit_items(&function.value);

        self.env.pop_segment();
    }

    // fn visit_return(&mut self, ret: &parsely_parser::item::Return, id: &NodeId) {

    // }

    fn visit_literal(&mut self, lit: &parsely_parser::expression::Literal, id: &NodeId) -> Type {
        let ty = match (lit, &self.type_hint) {
            (Literal::Bool(_), _) => Type::new_bool(),
            (
                Literal::Int(_),
                Some(
                    ty @ Type {
                        kind: TypeKind::Integer { .. },
                    },
                ),
            ) => ty.clone(),
            (Literal::Int(_), _) => Type::new_int(None),
            (
                Literal::Float(_),
                Some(
                    ty @ Type {
                        kind: TypeKind::Float { .. },
                    },
                ),
            ) => ty.clone(),
            (Literal::Float(_), _) => Type::new_float(None),
            (Literal::String(_), _) => Type::new_str(),
            (
                Literal::None(_),
                Some(
                    ty @ Type {
                        kind: TypeKind::Optional { .. },
                    },
                ),
            ) => ty.clone(),
            (Literal::None(_), _) => Type::new_unit(),
        };

        ty
    }

    fn visit_struct_init(
        &mut self,
        strct: &parsely_parser::expression::StructInit,
        id: &NodeId,
    ) -> Type {
        if let Some(path) = &strct.ty {
            let path = Path::from_path(path);
            let path = self.env.resolve_path(&path).expect("Unknown struct type");

            let node = self
                .path_to_node
                .get(&path)
                .expect("No path to node for path");

            let ty = self.node_to_type.get(node).expect("No type for node");
            let ty = ty.clone();

            match &ty.kind {
                TypeKind::Struct { .. } => {}
                _ => panic!("Expected struct type"),
            };

            self.node_to_type.insert(id.clone(), ty.clone());

            ty.clone()
        } else {
            eprintln!("WARN: struct path must be specified for now!");
            Type::default()
        }
    }

    fn visit_array_init(
        &mut self,
        array: &parsely_parser::expression::ArrayInit,
        id: &NodeId,
    ) -> Type {
        let new_ty_hint = if let Some(ty_hint) = &self.type_hint {
            let len = array.elements.value.len();

            match &ty_hint.kind {
                TypeKind::Infer => None,
                TypeKind::Array {
                    base,
                    size: Some(size),
                } if *size == len => Some(base.as_ref().clone()),
                TypeKind::Array { base, .. } => Some(base.as_ref().clone()),
                _ => panic!("Error trying to assign type array to type {ty_hint}"),
            }
        } else {
            None
        };

        let old_hint = std::mem::replace(&mut self.type_hint, new_ty_hint);

        let types: Vec<_> = array
            .elements
            .value
            .iter()
            .map(|e| self.visit_expr(e, &e.id))
            .collect();

        let _ = std::mem::replace(&mut self.type_hint, old_hint);

        for (a, b) in types.iter().zip(types.iter().skip(1)) {
            if a != b {
                panic!("Type error: array element types don't match")
            }
        }

        let array_ty = Type {
            kind: TypeKind::Array {
                base: Box::new(types[0].clone()),
                size: Some(types.len()),
            },
        };

        self.node_to_type.insert(id.clone(), array_ty.clone());

        array_ty
    }

    fn visit_deref(&mut self, deref: &parsely_parser::expression::Deref, id: &NodeId) -> Type {
        let expr_ty = self.visit_expr(&deref.expr, id);

        let base = expr_ty.deref_base();

        // todo: check these types
        self.node_to_type.insert(id.clone(), base.clone());

        base
    }

    fn visit_assign(&mut self, assign: &parsely_parser::expression::Assign, id: &NodeId) -> Type {
        let left_ty = self.visit_expr(&assign.left, id);
        let right_ty = self.visit_expr(&assign.right, id);

        // todo: check these types
        self.node_to_type.insert(id.clone(), left_ty);

        Type::default()
    }

    fn visit_binop(&mut self, binop: &parsely_parser::expression::BinOp, id: &NodeId) -> Type {
        let left_ty = self.visit_expr(&binop.left, id);
        let right_ty = self.visit_expr(&binop.right, id);

        let ty = match (&left_ty.kind, &right_ty.kind, &binop.op) {
            (
                TypeKind::Integer {
                    signed: ls,
                    size: None,
                },
                TypeKind::Integer {
                    signed: rs,
                    size: Some(_),
                },
                Tok![enum +]
                | Tok![enum -]
                | Tok![enum *]
                | Tok![enum /]
                | Tok![enum &]
                | Tok![enum |],
            ) if ls == rs => left_ty.clone(),
            (
                TypeKind::Integer {
                    signed: ls,
                    size: Some(_),
                },
                TypeKind::Integer {
                    signed: rs,
                    size: None,
                },
                Tok![enum +]
                | Tok![enum -]
                | Tok![enum *]
                | Tok![enum /]
                | Tok![enum &]
                | Tok![enum |],
            ) if ls == rs => left_ty.clone(),
            (
                TypeKind::Integer {
                    signed: ls,
                    size: lsz,
                },
                TypeKind::Integer {
                    signed: rs,
                    size: rsz,
                },
                Tok![enum +]
                | Tok![enum -]
                | Tok![enum *]
                | Tok![enum /]
                | Tok![enum &]
                | Tok![enum |],
            ) if ls == rs && lsz == rsz => left_ty.clone(),

            (
                TypeKind::Float { size: lsz },
                TypeKind::Float { size: rsz },
                Tok![enum +] | Tok![enum -] | Tok![enum *] | Tok![enum /],
            ) if lsz == rsz => left_ty.clone(),
            (
                lhs @ (TypeKind::Integer { .. }
                | TypeKind::Float { .. }
                | TypeKind::Bool
                | TypeKind::Char),
                rhs,
                Tok![enum >]
                | Tok![enum >=]
                | Tok![enum <=]
                | Tok![enum <]
                | Tok![enum ==]
                | Tok![enum !=],
            ) if lhs == rhs => Type::new_bool(),
            (TypeKind::Bool, TypeKind::Bool, Tok![enum &&] | Tok![enum ||]) => Type::new_bool(),
            _ => panic!("Type error! {} {:?} {}", left_ty, binop.op.as_str(), right_ty),
        };

        self.node_to_type.insert(id.clone(), ty.clone());

        ty
    }

    fn visit_addrof(&mut self, addrof: &parsely_parser::expression::AddrOf, id: &NodeId) -> Type {
        let base = self.visit_expr(&addrof.expr, id);
        let ty = Type::new_ref(base, addrof.mut_tok.is_some());

        self.node_to_type.insert(id.clone(), ty.clone());

        ty
    }

    fn visit_index(
        &mut self,
        base: &parsely_parser::expression::Expression,
        index: &parsely_parser::expression::Expression,
        id: &NodeId,
    ) -> Type {
        let base = self.visit_expr(base, id);
        let _index = self.visit_expr(index, id);

        let ty = base.index_result();

        self.node_to_type.insert(id.clone(), ty.clone());

        ty
    }

    fn visit_if(&mut self, if_expr: &parsely_parser::expression::If, id: &NodeId) -> Type {
        let if_ty = self.visit_items(&if_expr.body);

        let cond_ty = self.visit_expr(&if_expr.expr, id);

        match &cond_ty.kind {
            TypeKind::Bool => {}
            _ => panic!("Type error (expected bool)"),
        }

        let ty = if let Some(els) = &if_expr.else_clause {
            let else_ty = self.visit_items(&els.body);

            println!("{if_ty:?} {else_ty:?}");
            if if_ty == else_ty {
                if_ty
            } else {
                panic!("Type error!")
            }
        } else {
            Type::new_optional(if_ty)
        };

        self.node_to_type.insert(id.clone(), ty.clone());

        ty
    }

    fn visit_constdo(
        &mut self,
        constdo: &parsely_parser::expression::ConstDo,
        id: &NodeId,
    ) -> Type {
        let ty = self.visit_items(&constdo.body);

        self.node_to_type.insert(id.clone(), ty.clone());

        ty
    }

    fn visit_path(&mut self, path: &parsely_parser::expression::Path, id: &NodeId) -> Type {
        let path = Path::from_path(path);
        println!("{path:?} = {:?}", self.env.current_path);
        let path = self.env.resolve_path(&path).expect("TODO: Type erro");

        println!("{path:?}");

        let node = self.path_to_node.get(&path).expect("TODO: type error");
        self.node_to_path.insert(id.clone(), path);

        println!("{self:#?}");
        println!("{:?} {node:?} {:?}", node, self.node_to_type);

        let ty = self
            .node_to_type
            .get(node)
            .expect("TODO: type error")
            .clone();

        self.node_to_type.insert(id.clone(), ty.clone());

        ty
    }

    fn visit_var_decl(
        &mut self,
        var: &parsely_parser::item::ValueBinding,
        item: &parsely_parser::item::Item,
    ) {
        let ty = match (&var.init, &var.ty_annotation) {
            (Some((_, init)), Some((_, ty))) => {
                // TODO: check types are compattiple
                let ty = self.visit_type(ty);

                let (ty, _) =
                    self.with_type_hint(|resolver| resolver.visit_expr(init, &item.id), ty);

                ty
                // infer_type_with_expression(&ty, &expr_ty)
            }
            (Some((_, init)), None) => self.visit_expr(init, &item.id),
            (None, Some((_, ty))) => self.visit_type(ty),
            _ => panic!("Unable to infer type"),
        };

        self.node_to_type.insert(var.pattern.id.clone(), ty);
    }

    fn default_value(&mut self) -> Type {
        Type::default()
    }
}

fn infer_type_with_expression(ty: &Type, expr: &Type) -> Type {
    match (&ty.kind, &expr.kind) {
        (TypeKind::Infer, _) => expr.clone(),
        // (TypeKind::Integer { signed, size }, TypeKind::Integer { signed, size }) => expr.clone(),
        (
            TypeKind::Array {
                base: lbase,
                size: None,
            },
            TypeKind::Array {
                base: rbase,
                size: Some(size),
            },
        ) => {
            let base = infer_type_with_expression(lbase, &rbase);

            Type {
                kind: TypeKind::Array {
                    base: Box::new(base),
                    size: Some(*size),
                },
            }
        }
        (
            TypeKind::Array {
                base: lbase,
                size: Some(lsize),
            },
            TypeKind::Array {
                base: rbase,
                size: Some(rsize),
            },
        ) if lsize == rsize => {
            let base = infer_type_with_expression(lbase, &rbase);

            Type {
                kind: TypeKind::Array {
                    base: Box::new(base),
                    size: Some(*rsize),
                },
            }
        }
        (TypeKind::Slice { base: lbase }, TypeKind::Slice { base: rbase }) => {
            let base = infer_type_with_expression(lbase, &rbase);

            Type {
                kind: TypeKind::Slice {
                    base: Box::new(base),
                },
            }
        }
        (TypeKind::Tuple { fields: lfields }, TypeKind::Tuple { fields: rfields }) => {
            let fields = lfields
                .iter()
                .zip(rfields.iter())
                .map(|(l, r)| infer_type_with_expression(l, r))
                .collect();

            Type {
                kind: TypeKind::Tuple { fields },
            }
        }
        (
            TypeKind::Ref {
                base: lbase,
                mutable: lmut,
            },
            TypeKind::Ref {
                base: rbase,
                mutable: rmut,
            },
        ) if lmut == rmut => {
            let base = infer_type_with_expression(lbase, &rbase);

            Type {
                kind: TypeKind::Ref {
                    base: Box::new(base),
                    mutable: *lmut,
                },
            }
        }
        (
            TypeKind::IndexRef {
                base: lbase,
                mutable: lmut,
            },
            TypeKind::IndexRef {
                base: rbase,
                mutable: rmut,
            },
        ) if lmut == rmut => {
            let base = infer_type_with_expression(lbase, &rbase);

            Type {
                kind: TypeKind::IndexRef {
                    base: Box::new(base),
                    mutable: *lmut,
                },
            }
        }
        (TypeKind::Optional { base: lbase }, TypeKind::Optional { base: rbase }) => {
            let base = infer_type_with_expression(lbase, &rbase);

            Type {
                kind: TypeKind::Optional {
                    base: Box::new(base),
                },
            }
        }
        _ if ty == expr => ty.clone(),
        _ => panic!("Unable to infer type"),
    }
}
