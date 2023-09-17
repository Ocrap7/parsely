use parsely_parser::{expression, item, program::Program, types, NodeId, Parens};

pub trait Visitor<T = ()> {
    fn default_value(&mut self) -> T;

    fn visit_var_decl(&mut self, var: &item::ValueBinding, item: &item::Item) {}
    fn visit_fn_decl(&mut self, function: &item::FunctionBinding, item: &item::Item) {}
    fn visit_type_decl(&mut self, type_decl: &item::TypeAlias, item: &item::Item) {}
    fn visit_module_decl(&mut self, module: &item::Module, item: &item::Item) {}
    fn visit_import(&mut self, module: &item::Import, id: &NodeId) {}

    fn visit_path(&mut self, path: &expression::Path, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_index(
        &mut self,
        base: &expression::Expression,
        index: &expression::Expression,
        id: &NodeId,
    ) -> T {
        self.default_value()
    }
    fn visit_literal(&mut self, lit: &expression::Literal, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_array_init(&mut self, array: &expression::ArrayInit, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_tuple_init(&mut self, tuple: &expression::TupleInit, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_struct_init(&mut self, strct: &expression::StructInit, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_addrof(&mut self, addrof: &expression::AddrOf, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_deref(&mut self, deref: &expression::Deref, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_call(&mut self, call: &expression::Call, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_if(&mut self, if_expr: &expression::If, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_loop(&mut self, loop_expr: &expression::Loop, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_match(&mut self, match_expr: &expression::Match, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_constdo(&mut self, constdo: &expression::ConstDo, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_binop(&mut self, binop: &expression::BinOp, id: &NodeId) -> T {
        self.default_value()
    }
    fn visit_assign(&mut self, assign: &expression::Assign, id: &NodeId) -> T {
        self.default_value()
    }

    fn visit_type(&mut self, ty: &types::Type) -> T {
        self.default_value()
    }

    fn visit_program(&mut self, program: &Program) {
        for item in &program.items {
            self.visit_item(item);
        }
    }

    fn visit_items<'a>(&mut self, items: &[item::Item]) -> T {
        for item in &items[..items.len() - 1] {
            self.visit_item(item);
        }

        let last = items.last().expect("Expected last");
        self.visit_item(last)
    }

    fn visit_item(&mut self, item: &item::Item) -> T {
        match &item.kind {
            item::ItemKind::Binding(item::Binding::Function(func)) => {
                self.visit_fn_decl(func, item)
            }
            item::ItemKind::Binding(item::Binding::Value(var)) => self.visit_var_decl(var, item),
            item::ItemKind::TypeAlias(ty) => self.visit_type_decl(ty, item),
            item::ItemKind::Expression(expr) => {
                return self.visit_expr(expr, &item.id);
            }
            item::ItemKind::Continue(item::Continue {
                expr: Some(expr), ..
            }) => {
                self.visit_expr(expr.as_ref(), &item.id);
            }
            item::ItemKind::Break(item::Break {
                expr: Some(expr), ..
            }) => {
                self.visit_expr(expr.as_ref(), &item.id);
            }
            item::ItemKind::Return(item::Return {
                expr: Some(expr), ..
            }) => {
                self.visit_expr(expr.as_ref(), &item.id);
            }
            item::ItemKind::Import(import) => self.visit_import(import, &item.id),
            item::ItemKind::Module(module) => self.visit_module_decl(module, item),
            _ => (),
        }

        self.default_value()
    }

    fn visit_expr(&mut self, expr: &expression::Expression, id: &NodeId) -> T {
        match &expr.kind {
            expression::ExpressionKind::Literal(l) => self.visit_literal(l, &expr.id),
            expression::ExpressionKind::BinOp(b) => self.visit_binop(b, &expr.id),
            expression::ExpressionKind::Assign(b) => self.visit_assign(b, &expr.id),
            expression::ExpressionKind::Path(i) => self.visit_path(i, &expr.id),
            expression::ExpressionKind::Parens(i) => self.visit_expr(&i.value, &expr.id),
            expression::ExpressionKind::Index(i, ind) => {
                self.visit_index(i.as_ref(), &ind.value, &expr.id)
            }
            expression::ExpressionKind::ArrayInit(i) => self.visit_array_init(i, &expr.id),
            expression::ExpressionKind::TupleInit(i) => self.visit_tuple_init(i, &expr.id),
            expression::ExpressionKind::StructInit(i) => self.visit_struct_init(i, &expr.id),
            expression::ExpressionKind::AddrOf(i) => self.visit_addrof(i, &expr.id),
            expression::ExpressionKind::Deref(i) => self.visit_deref(i, &expr.id),
            expression::ExpressionKind::Call(i) => self.visit_call(i, &expr.id),
            expression::ExpressionKind::If(i) => self.visit_if(i, &expr.id),
            expression::ExpressionKind::Loop(i) => self.visit_loop(i, &expr.id),
            expression::ExpressionKind::Match(i) => self.visit_match(i, &expr.id),
            expression::ExpressionKind::ConstDo(i) => self.visit_constdo(i, &expr.id),
            expression::ExpressionKind::Type(i) => self.visit_type(i.as_ref()),
            expression::ExpressionKind::Poison => self.default_value(),
        }
    }
}
