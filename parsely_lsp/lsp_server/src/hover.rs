use std::{cmp::Ordering, collections::HashMap};

use parsely_gen_asm::module::Module;
use parsely_lexer::{AsSpan, Position, SearchPosition};
use parsely_parser::{
    expression::Expression,
    item::{Binding, BoundValue, Item, Op},
    program::Program,
};
use tower_lsp::lsp_types::{Hover, HoverContents, MarkedString, MarkupContent, MarkupKind, Range};

pub struct HoverCache(HashMap<Range, Option<Hover>>);

pub struct HoverProvider<'a> {
    module: &'a Module,
}

impl<'a> HoverProvider<'a> {
    pub fn new(module: &'a Module) -> HoverProvider<'a> {
        HoverProvider { module }
    }
}

impl HoverProvider<'_> {
    pub fn run(&self, program: &Program, position: &Position) -> Option<Option<Hover>> {
        program
            .as_span()
            .contains_position(position)
            .then_some(())?;

        let item_index = program.items.search_position(position)?;

        self.run_item(&program.items[item_index], position)
    }

    pub fn run_item(&self, item: &Item, position: &Position) -> Option<Option<Hover>> {
        match item {
            Item::Binding(bind) => self.run_binding(bind, position),
            Item::Attribute(_) => None,
            Item::Poison => None,
        }
    }

    pub fn run_binding(&self, binding: &Binding, position: &Position) -> Option<Option<Hover>> {
        if binding.value.as_span().contains_position(position) {
            match &binding.value {
                BoundValue::OpList(ops) => {
                    if let Some(res) = ops.list.iter().find_map(|f| self.run_op(f, position)) {
                        return Some(res);
                    }
                }
                _ => (),
            }
        } else if binding.ident.span.contains_position(position) {
            if let Ok(sym) = self.module.find_symbol(&binding.ident) {
                if let Some(docs) = &sym.node.docs {
                    let doc_str = docs
                        .iter()
                        .map(|doc| doc.value.as_str())
                        .collect::<Vec<_>>()
                        .join("\n");

                    let instructions = if let BoundValue::OpList(o) = &binding.value {
                        format!(" // {} instructions", o.list.len())
                    } else {
                        "".to_string()
                    };

                    return Some(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                r#"```par
let {} = {};{}
```
---
{doc_str}
"#,
                                binding.ident.value,
                                sym.value.to_string(),
                                instructions,
                            ),
                        }),
                        range: None,
                    }));
                }
            }
        }

        None
    }

    pub fn run_op(&self, op: &Op, position: &Position) -> Option<Option<Hover>> {
        if op.op.as_span().contains_position(position) {
            if let Some(instr) = self.module.pack.instructions.get(&op.op.value) {
                if let Some(doc) = &instr.doc {
                    let syntax = format!("{} {}", op.op.value, instr.input.join(" "));

                    return Some(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                r#"```par
{syntax}
```
---
{doc}
"#
                            ),
                        }),
                        range: None,
                    }));
                }
            }

            Some(None)
        } else if op.args.as_span().contains_position(position) {
            self.run_expr(&op.args, position)
        } else {
            None
        }
    }

    fn run_expr(&self, expr: &Expression, position: &Position) -> Option<Option<Hover>> {
        match expr {
            Expression::BinOp(op) => {
                if op.left.as_span().contains_position(position) {
                    return self.run_expr(&op.left, position);
                }

                if op.right.as_span().contains_position(position) {
                    return self.run_expr(&op.right, position);
                }
            }
            Expression::Parens(p) => return self.run_expr(&p.value, position),
            Expression::Ident(ident) => {
                if let Some(k) = self.module.pack.registers.get(&ident.value) {
                    return Some(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: format!(
                                r#"
#### Register `{}`
{}
#### `{}` bits
{}
"#,
                                ident.value,
                                k.subset
                                    .as_ref()
                                    .map(|f| format!("\nsubset of `{}`\n\n", f))
                                    .unwrap_or("".to_string()),
                                k.size.unwrap(),
                                k.description
                                    .as_ref()
                                    .map(|s| format!("\n---\n{}", s))
                                    .unwrap_or("".to_string()),
                            ),
                        }),
                        range: None,
                    }));
                } else if let Ok(sym) = self.module.find_symbol(ident) {
                    if let Some(docs) = &sym.node.docs {
                        let doc_str = docs
                            .iter()
                            .map(|doc| doc.value.as_str())
                            .collect::<Vec<_>>()
                            .join("\n");

                        return Some(Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: format!(
                                    r#"```par
let {} = {};
```

---

{doc_str}
"#,
                                    ident.value,
                                    sym.value.to_string()
                                ),
                            }),
                            range: None,
                        }));
                    }
                }
            }
            _ => (),
        }

        Some(None)
    }
}
