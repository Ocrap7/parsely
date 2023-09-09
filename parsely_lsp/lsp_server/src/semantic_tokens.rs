use parsely_gen_asm::{module::Module, value::SymbolValue};
use parsely_lexer::{AsSpan, Span};
use parsely_parser::{
    expression::{Expression, Literal},
    item::{BoundValue, Item, Op},
    program::Program,
};
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, MarkupContent, SemanticToken,
    SemanticTokenModifier, SemanticTokenType,
};

macro_rules! define_types {
    ($($ty:ident),*) => {
        pub const STOKEN_TYPES: &[SemanticTokenType] = &[
            $(SemanticTokenType::$ty),*
        ];

        define_types!{@(0; $($ty),*,)}
    };
    (@($idx:expr; $ty:ident, $($rest:tt)*) $($arms:tt)*) => {
        define_types!{
            @(1 + $idx; $($rest)*)
            $($arms)*
            [$ty] => { $idx };
        }
    };
    (@($idx:expr; $(,)?) $($arms:tt)*) => {
        macro_rules! semantic_type {
            $($arms)*
        }
    };
}

macro_rules! define_mods {
    ($($ty:ident : $ex:expr),*) => {
        pub const STOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
            $($ex),*
        ];

        define_mods!{@(1; $($ty),*,)}
    };
    (@($idx:expr; $ty:ident, $($rest:tt)*) $($arms:tt)*) => {
        define_mods!{
            @(1 + $idx; $($rest)*)
            $($arms)*
            [$ty] => { $idx };
        }
    };
    (@($idx:expr; $(,)?) $($arms:tt)*) => {
        macro_rules! semantic_mod {
            $($arms)*
        }
    };
}

define_types! {
    KEYWORD,
    TYPE,
    VARIABLE,
    NAMESPACE,
    CLASS,
    ENUM,
    INTERFACE,
    STRUCT,
    TYPE_PARAMETER,
    PARAMETER,
    PROPERTY,
    ENUM_MEMBER,
    EVENT,
    FUNCTION,
    METHOD,
    MACRO,
    MODIFIER,
    COMMENT,
    STRING,
    NUMBER,
    REGEXP,
    OPERATOR
}

define_mods! {
    CONST : SemanticTokenModifier::new("const"),
    READONLY : SemanticTokenModifier::new("readonly")
}

#[derive(Default)]
pub struct SemanticTokenBuilder {
    tokens: Vec<SemanticToken>,
    last_line: u32,
    last_pos: u32,
}

impl SemanticTokenBuilder {
    pub fn push(&mut self, line: u32, position: u32, length: u32, token: u32, modifier: u32) {
        if self.last_line == line {
            let delta_pos = position - self.last_pos;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line: 0,
                delta_start: delta_pos,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        } else {
            let delta_line = line - self.last_line;
            self.last_line = line;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line,
                delta_start: position,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        }
    }

    pub fn push_token<T: AsSpan>(
        &mut self,
        token: &T,
        semantic_type_index: u32,
        semantic_modifier_index: u32,
    ) {
        let span = token.as_span();

        self.push(
            span.start.line as u32,
            span.start.column as u32,
            span.len() as u32,
            semantic_type_index,
            semantic_modifier_index,
        )
    }
}

macro_rules! brr {
            ($($t:ident),*) => {
                [$(
                    (CompletionItemKind::$t, stringify!($t))
                ),*]
            };
        }
pub struct SemanticGenerator<'a> {
    module: &'a Module,
    tokens: SemanticTokenBuilder,
    completes: Vec<(CompletionItem, Span)>,
}

impl<'a> SemanticGenerator<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module,
            tokens: SemanticTokenBuilder::default(),
            completes: Vec::new(),
        }
    }

    pub fn run(&mut self, program: &Program) {
        for item in program.items.iter() {
            self.run_item(item);
        }
    }

    pub fn finish(self) -> (Vec<SemanticToken>, Vec<(CompletionItem, Span)>) {
        (self.tokens.tokens, self.completes)
    }

    fn complete_registers(&self, span: Span) -> Vec<(CompletionItem, Span)> {
        self.module
            .pack
            .registers
            .keys()
            .map(|instr| {
                (
                    CompletionItem {
                        label: instr.clone(),
                        kind: Some(CompletionItemKind::FIELD),
                        // sort_text
                        ..Default::default()
                    },
                    span,
                )
            })
            .collect()
    }

    fn complete_instructions(&self, span: Span) -> Vec<(CompletionItem, Span)> {
        self.module
            .pack
            .instructions
            .keys()
            .map(|instr| {
                (
                    CompletionItem {
                        label: instr.clone(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                    span,
                )
            })
            .collect()
    }
}

impl SemanticGenerator<'_> {
    fn run_item(&mut self, item: &Item) {
        match item {
            Item::Binding(bind) => {
                self.tokens
                    .push_token(&bind.let_tok, semantic_type![KEYWORD], 0);

                // self.completes.push((
                //     CompletionItem {
                //         label: "poo".to_string(),
                //         ..Default::default()
                //     },
                //     bind.let_tok.as_span().join(bind.semi_tok.as_span()),
                // ));

                match &bind.value {
                    BoundValue::Expression(expr) => {
                        self.tokens.push_token(
                            &bind.ident,
                            semantic_type![VARIABLE],
                            semantic_mod![CONST],
                        );

                        self.run_expr(expr);
                    }
                    BoundValue::OpList(op) => {
                        self.tokens
                            .push_token(&bind.ident, semantic_type![FUNCTION], 0);

                        for (op, span) in op.list.iter().zip(
                            op.list
                                .iter_punct()
                                .map(|f| f.as_span())
                                .chain([bind.semi_tok.as_span()].into_iter()),
                        ) {
                            self.run_op(op, span);
                        }

                        let eq = bind.eq_tok.as_span();
                        let first = op
                            .list
                            .iter()
                            .next()
                            .map_or(bind.semi_tok.as_span(), |f| f.op.span);

                        // iterates ranges between operator lists and between equals token and first op (or semi)
                        for (this, next) in [(eq, first)].into_iter().chain(
                            op.list.iter_punct().map(|p| p.as_span()).zip(
                                op.list
                                    .iter()
                                    .skip(1)
                                    .map(|f| f.op.as_span())
                                    .chain([bind.semi_tok.as_span()]),
                            ),
                        ) {
                            self.completes
                                .extend(self.complete_instructions(this.increment().join(next)));
                        }
                    }
                }
            }
            Item::Attribute(attr) => {
                self.run_expr(&attr.value.value);
            }
            _ => todo!(),
        }
    }

    fn run_op(&mut self, op: &Op, next_span: Span) {
        // Only add semantic value if operation is defined in pack
        if self.module.pack.instructions.contains_key(&op.op.value) {
            self.tokens.push_token(&op.op, semantic_type![KEYWORD], 0);
        }

        // self.completes
        //     .extend(self.complete_registers(op.args.as_span()));

        if let Some(instruction) = self.module.pack.instructions.get(&op.op.value) {
            let tys = instruction.get_types(
                &self.module.pack,
                &op.op.value,
                &op.args,
                op.op.as_span(),
                next_span,
            );

            let tys: Vec<_> = tys
                .into_iter()
                .filter_map(|(ty, _, span)| match ty {
                    "reg" => Some(self.complete_registers(span)),
                    "imm" => Some(
                        self.module
                            .iter_symbols()
                            .filter(|(_, sym)| match sym.value {
                                SymbolValue::Poison | SymbolValue::Function() => false,
                                _ => true,
                            })
                            .map(|(name, sym)| {
                                (
                                    CompletionItem {
                                        label: name.clone(),
                                        kind: Some(CompletionItemKind::CONSTANT),
                                        detail: Some(sym.value.type_str().to_string()),
                                        documentation: Some(Documentation::MarkupContent(
                                            MarkupContent {
                                                kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                                                value: format!(
                                                    "Constant `{}` of type `{}` with value `{}`.\n{} usages",
                                                    name,
                                                    sym.value.type_str(),
                                                    sym.value,
                                                    sym.usages.load(std::sync::atomic::Ordering::SeqCst),
                                                )
                                            }
                                        )),
                                        ..Default::default()
                                    },
                                    span,
                                )
                            })
                            .collect::<Vec<_>>(),
                    ),
                    //Some(
                    // (brr! {
                    //     TEXT,
                    //     METHOD,
                    //     FUNCTION,
                    //     CONSTRUCTOR,
                    //     FIELD,
                    //     VARIABLE,
                    //     CLASS,
                    //     INTERFACE,
                    //     MODULE,
                    //     PROPERTY,
                    //     UNIT,
                    //     VALUE,
                    //     ENUM,
                    //     KEYWORD,
                    //     SNIPPET,
                    //     COLOR,
                    //     FILE,
                    //     REFERENCE,
                    //     FOLDER,
                    //     ENUM_MEMBER,
                    //     CONSTANT,
                    //     STRUCT,
                    //     EVENT,
                    //     OPERATOR,
                    //     TYPE_PARAMETER
                    // })
                    // .into_iter()
                    // .map(|k| {
                    //     (
                    //         CompletionItem {
                    //             label: k.1.to_ascii_lowercase(),
                    //             kind: Some(k.0),
                    //             ..Default::default()
                    //         },
                    //         span,
                    //     )
                    // })
                    // .collect(),
                    // vec![(
                    //     CompletionItem {
                    //         label: "Some constant".to_string(),
                    //         ..Default::default()
                    //     },
                    //     span,
                    // )],
                    //),
                    _ => None,
                })
                // .inspect(|f| {
                //     println!("Items ({}): ", op.op.value);
                //     f.iter().for_each(|f| {
                //         println!("{} {}", f.0.label, f.1);
                //     });
                //     println!();
                // })
                .flatten()
                .collect();

            self.completes.extend(tys);
        }

        self.run_expr(&op.args);
    }

    fn run_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Ident(ident) => {
                if self.module.pack.registers.contains_key(&ident.value) {
                    self.tokens.push_token(ident, semantic_type![TYPE], 0);
                } else if let Ok(sym) = self.module.find_symbol(ident) {
                    self.tokens
                        .push_token(ident, semantic_type![VARIABLE], semantic_mod!(CONST))
                }
            }
            Expression::BinOp(bin_op) => {
                self.run_expr(&bin_op.left);
                self.run_expr(&bin_op.right);
            }
            Expression::Literal(lit) => match lit {
                Literal::Int(i) => self.tokens.push_token(&i.value, semantic_type![NUMBER], 0),
                Literal::Float(i) => self.tokens.push_token(&i.value, semantic_type![NUMBER], 0),
                Literal::String(i) => self.tokens.push_token(&i.value, semantic_type![STRING], 0),
            },
            _ => (),
        }
    }
}
