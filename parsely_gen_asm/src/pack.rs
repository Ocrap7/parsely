use std::{collections::HashMap, fmt::Write};

use colored::Colorize;
use parsely_lexer::{AsSpan, Span};
use parsely_parser::expression::Expression;
use serde::{de, Deserialize};

use crate::value::Value;

use parsely_diagnostics::{Diagnostic, DiagnosticLevel, Result};

#[derive(Debug, Deserialize)]
pub struct Pack {
    #[serde(deserialize_with = "deserialize_range")]
    pub registers: HashMap<String, Register>,
    pub instructions: HashMap<String, Instruction>,
}

impl Pack {
    pub fn get_instruction_syntax(&self, instruction: &str) -> Option<String> {
        let Some(instr) = self.instructions.get(instruction) else {
            return None;
        };

        // instr.
        None
    }

    pub fn instruction_doc(&self, instruction: &str) -> Option<&str> {
        self.instructions
            .get(instruction)
            .and_then(|instr| instr.doc.as_ref().map(String::as_str))
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct Register {
    pub subset: Option<String>,
    pub description: Option<String>,
    pub size: Option<u64>,
}

fn deserialize_range<'de, D>(
    deserializer: D,
) -> std::result::Result<HashMap<String, Register>, D::Error>
where
    D: de::Deserializer<'de>,
{
    let mut map = HashMap::<String, Register>::deserialize(deserializer).map(|value| {
        HashMap::from_iter(
            value
                .into_iter()
                .map(|(key, value)| {
                    if key.contains('-') {
                        // key contains "w0-w10"

                        let (start, end) = key.split_once('-').unwrap();
                        //  (start: "w0", end "w10")

                        let pivot = start.find(|c: char| c.is_ascii_digit()).unwrap();
                        let (repeat_start, start_digits_str) = start.split_at(pivot);
                        //  (repeat_start: "w", start_digits_str: "0")

                        let pivot = end.find(|c: char| c.is_ascii_digit()).unwrap();
                        let (repeat_end, end_digits_str) = end.split_at(pivot);
                        //  (repeat_end: "w", end_digits_str: "10")

                        assert_eq!(repeat_start, repeat_end);

                        let start_digits: usize = start_digits_str.parse().unwrap();
                        //  start_digits: 0
                        let end_digits: usize = end_digits_str.parse().unwrap();
                        //  start_digits: 10

                        let vec = (start_digits..=end_digits)
                            .map(|i| {
                                let key = format!("{repeat_start}{i}");
                                let mut value = value.clone();

                                // if subset also is a range, we break it up as well and use the current i as the index
                                if let Some(subset) = &value.subset {
                                    if subset.contains("-") {
                                        let (start, end) = subset.split_once('-').unwrap();
                                        //  (start: "x0", end "x10")

                                        let pivot =
                                            start.find(|c: char| c.is_ascii_digit()).unwrap();
                                        let (subset_repeat_start, subset_start_digits_str) =
                                            start.split_at(pivot);
                                        //  (subset_repeat_start: "x", subset_start_digits_str: "0")

                                        let pivot = end.find(|c: char| c.is_ascii_digit()).unwrap();
                                        let (subset_repeat_end, subset_end_digits_str) =
                                            end.split_at(pivot);
                                        //  (subset_repeat_end: "x", subset_end_digits_str: "10")

                                        assert_eq!(subset_repeat_start, subset_repeat_end);

                                        let subset_start_digits: usize =
                                            subset_start_digits_str.parse().unwrap();
                                        //  subset_start_digits: 0

                                        let subset_end_digits: usize =
                                            subset_end_digits_str.parse().unwrap();
                                        //  subset_start_digits: 10

                                        assert_eq!(subset_start_digits, start_digits);
                                        assert_eq!(subset_end_digits, end_digits);

                                        value.subset = Some(format!("{subset_repeat_start}{i}"));
                                    }
                                }

                                (key, value)
                            })
                            .collect::<Vec<_>>();

                        vec.into_iter()
                    } else {
                        vec![(key, value)].into_iter()
                    }
                })
                .flatten(),
        )
    });

    // Inherit properties for subset values by copying them over if they don't exist in the subset already
    if let Ok(map) = &mut map {
        let values_to_change: Vec<_> = map
            .iter()
            .filter_map(|(key, value)| {
                if let Some(subset) = &value.subset {
                    if let Some(parent_value) = map.get(subset) {
                        return Some((key.clone(), parent_value.clone()));
                    }
                }

                None
            })
            .collect();

        for (key, value) in values_to_change {
            let subset_value = map.get_mut(key.as_str()).unwrap();

            let subset = subset_value.subset.take();

            *subset_value = Register {
                subset,
                description: subset_value
                    .description
                    .as_ref()
                    .or(value.description.as_ref())
                    .cloned(),
                size: subset_value.size.or(value.size),
            };
        }
    }

    map
}

fn trans<'a>(
    pack: &Pack,
    input_iter: &mut impl Iterator<Item = &'a String>,
    vars: &mut HashMap<String, Value>,
    expr: &Expression,
    name: &str,
) -> Result<()> {
    match expr {
        Expression::Ident(ident) => {
            let var = input_iter.next().ok_or_else(|| {
                Diagnostic::Message(
                    "The provided expression does not match the selected pack for operation".into(),
                    expr.as_span(),
                    DiagnosticLevel::Error,
                )
            })?;

            let (ty, value) = var.split_once(":").ok_or_else(|| {
                Diagnostic::PackFile(
                    format!("input for instruction `{}` didn't contain a type", name),
                    DiagnosticLevel::Error,
                )
            })?;

            match ty {
                "reg" => {
                    if pack.registers.contains_key(&ident.value) {
                        vars.insert(value.to_string(), Value::Register(ident.value.to_string()));
                    } else {
                        return Err(Diagnostic::Message(
                                    format!(
                                        "input `{}` for instruction `{}` references a register ({}) that was not defined",
                                        value,
                                        name,
                                        ident.value,
                                    ),
                                    ident.span.clone(),
                                    DiagnosticLevel::Error,
                                ));
                    }
                }
                "imm" => {
                    vars.insert(value.to_string(), Value::Constant(ident.value.to_string()));
                    // TODO: check if ident exists in symbol table
                }
                _ => {
                    return Err(Diagnostic::PackFile(
                        format!(
                            "unknown type `{}` on input `{}` for instruction `{}`",
                            ty, value, name,
                        ),
                        DiagnosticLevel::Error,
                    ))
                }
            }
        }
        Expression::BinOp(op) => {
            trans(pack, input_iter, vars, &op.left, name)?;

            match input_iter.next() {
                Some(val) if val == op.op.as_str() => {}
                Some(val) => {
                    return Err(Diagnostic::Message(
                        format!("Expected operator {} but found {}", op.op.as_str(), val),
                        expr.as_span(),
                        DiagnosticLevel::Error,
                    ))
                }
                None => {
                    return Err(Diagnostic::Message(
                        format!("Operator {} was found but was not expected", op.op.as_str()),
                        Span::join_all([op.op.as_span(), op.right.as_span()]),
                        DiagnosticLevel::Error,
                    ))
                }
            }

            trans(pack, input_iter, vars, &op.right, name)?;
        }
        Expression::Poison => (),
        _ => unimplemented!("Expression not supported"),
    }

    Ok(())
}

#[derive(Debug, Deserialize)]
pub struct Instruction {
    pub input: Vec<String>,
    pub output: Vec<String>,
    pub doc: Option<String>,
}

impl Instruction {
    /// Returns iterator of (type, name) for each expected input in pack (for instruction)
    pub fn get_types<'b>(
        &'b self,
        pack: &Pack,
        instrucion: &str,
        expr: &Expression,
        start_anchor: Span,
        end_anchor: Span,
    ) -> Vec<(&str, &str, Span)> {
        let mut input_iter = self.input.iter();
        let mut vars = Vec::new();

        fn trans<'a: 'b, 'b>(
            pack: &Pack,
            input_iter: &mut impl Iterator<Item = &'a String>,
            vars: &mut Vec<(&'b str, &'b str, Span)>,
            expr: &Expression,
            name: &str,
            span: Span,
        ) {
            match expr {
                Expression::BinOp(op) => {
                    trans(
                        pack,
                        input_iter,
                        vars,
                        &op.left,
                        name,
                        span.start.join(op.op.as_span().end),
                    );
                    input_iter.next();
                    trans(
                        pack,
                        input_iter,
                        vars,
                        &op.right,
                        name,
                        op.op.as_span().start.join(span.end),
                    );
                }
                _ => {
                    let Some(input) = input_iter.next() else {
                        eprintln!("{}", "no input".on_red());
                        return
                    };

                    let Some((ty, value)) = input.split_once(":") else {
                        eprintln!("{} {}", "no type".on_red(), input);
                        return;
                    };

                    vars.push((ty, value, span));
                }
            }
        }

        trans(
            pack,
            &mut input_iter,
            &mut vars,
            expr,
            instrucion,
            start_anchor.join(end_anchor),
        );

        vars
    }

    pub fn transform(&self, pack: &Pack, name: &str, expr: &Expression) -> Result<String> {
        let mut input_iter = self.input.iter();
        let mut vars = HashMap::with_capacity(8);

        trans(pack, &mut input_iter, &mut vars, expr, name)?;

        let mut string = String::with_capacity(32);

        let mut out_iter = self.output.iter();
        let first = out_iter.next().ok_or_else(|| {
            Diagnostic::PackFile(
                format!(
                    "instruciton `{}` does not specify at least one output instruction",
                    name
                ),
                DiagnosticLevel::Error,
            )
        })?;

        write!(string, "{} ", first)?;

        for (i, out) in out_iter.enumerate() {
            if let Some(var) = vars.get(out) {
                write!(string, "{}", var)?;
            } else {
                return Err(Diagnostic::Message(
                    format!(
                        "instruction `{}` expected arguments `{}` but only received `{}`",
                        name,
                        self.input.join(", "),
                        vars.keys().cloned().collect::<Vec<_>>().join(", "),
                    ),
                    expr.as_span(),
                    DiagnosticLevel::Error,
                ));
            }

            if i != self.output.len() - 2 {
                write!(string, ", ")?;
            }
        }

        Ok(string)
    }
}
