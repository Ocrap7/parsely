use std::{collections::HashSet, str::FromStr};

use proc_macro::TokenTree;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream, Parser},
    parse_macro_input,
    punctuated::Punctuated,
    Attribute, Data, DeriveInput, Fields, GenericParam, Type,
};

fn has_attr<'a>(attrs: &'a [Attribute], st: &[&str]) -> Option<&'a Attribute> {
    attrs.iter().find(|a| {
        let segs = &a.meta.path().segments;
        if segs.len() != st.len() {
            return false;
        }

        for (a, b) in segs.iter().zip(st.iter()) {
            if a.ident != *b {
                return false;
            }
        }

        true
    })
}

#[derive(Debug)]
enum ArgType {
    Ident(String),
    Type(Type),
}

impl Parse for ArgType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(syn::Token![type]) {
            let _type: syn::Token![type] = input.parse()?;
            let _eq: syn::Token![=] = input.parse()?;
            Ok(ArgType::Type(input.parse()?))
        } else {
            let ident: syn::Ident = input.parse()?;
            Ok(ArgType::Ident(ident.to_string()))
        }
    }
}

fn attr_args(attr: &Attribute) -> (HashSet<String>, HashSet<Type>) {
    match &attr.meta {
        syn::Meta::List(list) => {
            let parser = Punctuated::<ArgType, syn::Token![,]>::parse_separated_nonempty;
            let args = parser.parse(list.tokens.clone().into()).unwrap();

            let all: (HashSet<_>, HashSet<_>) = args
                .into_iter()
                .map(|f| match f {
                    ArgType::Ident(ident) => (Some(ident), None),
                    ArgType::Type(ty) => (None, Some(ty)),
                })
                .unzip();

            (
                all.0.into_iter().flatten().collect(),
                all.1.into_iter().flatten().collect(),
            )
        }
        _ => (HashSet::new(), HashSet::new()),
    }
}

fn is_optional(ty: &Type) -> bool {
    match &ty {
        syn::Type::Path(path) => {
            let segs = &path.path.segments;
            if segs.len() != 1 {
                return false;
            }
            segs.first().unwrap().ident == "Option"
        }
        _ => false,
    }
}

#[proc_macro]
pub fn str_arr(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut iter = tokens.into_iter();

    let Some(TokenTree::Literal(lit)) = iter.next() else {
        panic!("Expected string!")
    };

    let lit_string = lit.to_string();

    let arr = lit_string
        .chars()
        .skip(1)
        .take(lit_string.len() - 2)
        .map(|l| l.to_string())
        .collect::<Vec<_>>()
        .join("', '");

    let fstr = format!("['{}']", arr);

    proc_macro::TokenStream::from_str(&fstr).unwrap()
}

fn ident_to_array<I: ToString>(ident: I, append: Option<&str>) -> String {
    let mut lit_string = ident.to_string();
    if let Some(append) = append {
        lit_string.push_str(append);
    }

    let arr = lit_string
        .chars()
        // .take(lit_string.len() - 1)
        .map(|l| l.to_string())
        .collect::<Vec<_>>()
        .join("', '");

    format!("['{}']", arr)
}

#[proc_macro]
pub fn str_arr_noun(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut iter = tokens.into_iter();

    let Some(TokenTree::Literal(lit)) = iter.next() else {
        panic!("Expected string!")
    };

    let arr = ident_to_array(lit, None);

    proc_macro::TokenStream::from_str(&arr).unwrap()
}

#[proc_macro]
pub fn consume_kw(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut iter = tokens.into_iter();

    let Some(TokenTree::Ident(ident)) = iter.next() else {
        panic!("Expected ident!")
    };

    let fstr = format!(
        "Some({}::from_span_start(self.make_position(), kw_slice.len(), KeywordMod::empty()))",
        ident
    );

    proc_macro::TokenStream::from_str(&fstr).unwrap()
}

mod kw {
    syn::custom_keyword!(other);
    syn::custom_keyword!(noun);
    syn::custom_keyword!(plural);
    syn::custom_keyword!(verb);
    syn::custom_keyword!(thirdp);
    syn::custom_keyword!(pastparti);
    syn::custom_keyword!(gerund);
}

enum MatchArm {
    Noun {
        noun: kw::noun,
        ident: syn::LitStr,
        plural: Option<(syn::Token![,], kw::plural, syn::Token![:], syn::LitStr)>,
        fat: syn::Token![=>],
        output: syn::Ident,
    },
    Verb {
        verb: kw::verb,
        ident: syn::LitStr,
        thirdp: Option<(syn::Token![,], kw::thirdp, syn::Token![:], syn::LitStr)>,
        pastparti: Option<(syn::Token![,], kw::pastparti, syn::Token![:], syn::LitStr)>,
        gerund: Option<(syn::Token![,], kw::gerund, syn::Token![:], syn::LitStr)>,
        fat: syn::Token![=>],
        output: syn::Ident,
    },
    Other {
        other: kw::other,
        ident: syn::LitStr,
        fat: syn::Token![=>],
        output: syn::Ident,
    },
    Identity {
        arm: syn::Arm,
    },
}

impl Parse for MatchArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(kw::noun) {
            let noun = input.parse()?;
            let ident = input.parse()?;

            let plural = if input.peek(kw::plural) {
                Some((
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                ))
            } else {
                None
            };

            Ok(Self::Noun {
                noun,
                ident,
                plural,
                fat: input.parse()?,
                output: input.parse()?,
            })
        } else if input.peek(kw::verb) {
            let verb = input.parse()?;
            let ident = input.parse()?;

            let thirdp = if input.peek(kw::thirdp) {
                Some((
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                ))
            } else {
                None
            };

            let pastparti = if input.peek(kw::pastparti) {
                Some((
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                ))
            } else {
                None
            };

            let gerund = if input.peek(kw::gerund) {
                Some((
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                    input.parse()?,
                ))
            } else {
                None
            };

            Ok(Self::Verb {
                verb,
                ident,
                thirdp,
                pastparti,
                gerund,
                fat: input.parse()?,
                output: input.parse()?,
            })
        } else if input.peek(kw::other) {
            Ok(Self::Other {
                other: input.parse()?,
                ident: input.parse()?,
                fat: input.parse()?,
                output: input.parse()?,
            })
        } else {
            Ok(Self::Identity {
                arm: input.parse()?,
            })
        }
    }
}

struct MatchInput {
    match_tok: syn::Token![match],
    ident: syn::Ident,
    _semi: syn::Token![;],

    arms: Punctuated<MatchArm, syn::Token![,]>,
}

impl Parse for MatchInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(MatchInput {
            match_tok: input.parse()?,
            ident: input.parse()?,
            _semi: input.parse()?,
            arms: Punctuated::parse_terminated(input)?,
        })
    }
}

// fn is_vowel(c: char) -> bool {
//     matches!(c, 'a' | )
// }

#[proc_macro]
pub fn match_words(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let MatchInput {
        match_tok,
        ident,
        arms,
        ..
    } = syn::parse_macro_input!(tokens as MatchInput);

    let arms = arms.into_iter().map(|arm| match arm {
        MatchArm::Noun {
            noun, ident, fat, output, plural, ..
        } => {
            // Write irregular plural
            let plural = plural.map(|plural| ident_to_array(plural.3.value(), None));
            let arr = ident_to_array(ident.value(), None);
            let let_token = syn::token::Let {
                span: noun.span,
            };

            let output_body = quote! {{
                #let_token mods = KeywordMod::NOUN;
                Some(#output::from_span_start(self.make_position(), kw_slice.len(), mods))
            }};

            let left = TokenStream::from_str(&arr).unwrap();
            let first = quote! {
                #left #fat #output_body
            };

            if let Some(plural) = plural {
                let left = TokenStream::from_str(&plural).unwrap();
                let output_body = quote! {
                    Some(#output::from_span_start(self.make_position(), kw_slice.len(), KeywordMod::NOUN | KeywordMod::PLURAL))
                };
                quote! {
                    #first,
                    #left #fat #output_body
                }
            } else {
                let arr = ident_to_array(ident.value(), Some("s"));
                let left = TokenStream::from_str(&arr).unwrap();
                let output_body = quote! {
                    Some(#output::from_span_start(self.make_position(), kw_slice.len(), KeywordMod::NOUN | KeywordMod::PLURAL))
                };

                quote! {
                    #first,
                    #left #fat #output_body
                }
            }
        }
        MatchArm::Verb { verb, ident, thirdp, pastparti, gerund, fat, output } => {
            let thirdp = thirdp
                .map(|thirdp| ident_to_array(thirdp.3.value(), None))
                .unwrap_or_else(|| ident_to_array(ident.value(), Some("s")));

            let pastparti = pastparti
                .map(|pastparti| ident_to_array(pastparti.3.value(), None))
                .unwrap_or_else(|| {
                    let val = ident.value();
                    if val.ends_with('e') {
                        ident_to_array(ident.value(), Some("d"))
                    } else {
                        ident_to_array(ident.value(), Some("ed"))
                    }
                });

            let gerund = gerund
                .map(|gerund| ident_to_array(gerund.3.value(), None))
                .unwrap_or_else(|| {
                    let val = ident.value();
                    if val.ends_with('e') {
                        ident_to_array(&val[..val.len() - 1], Some("ing"))
                    } else {
                        ident_to_array(val, Some("ing"))
                    }
                });

            let arr = ident_to_array(ident.value(), None);
            let let_token = syn::token::Let {
                span: verb.span,
            };

            let left = TokenStream::from_str(&arr).unwrap();
            let mut first = quote! {
                #left #fat {
                    #let_token mods = KeywordMod::VERB;
                    Some(#output::from_span_start(self.make_position(), kw_slice.len(), mods))
                }
            };

            let left = TokenStream::from_str(&thirdp).unwrap();
            first.extend(quote! {
                ,#left #fat Some(#output::from_span_start(self.make_position(), kw_slice.len(), KeywordMod::VERB | KeywordMod::THIRD_PERSON))
            });

            let left = TokenStream::from_str(&pastparti).unwrap();
            first.extend(quote! {
                ,#left #fat Some(#output::from_span_start(self.make_position(), kw_slice.len(), KeywordMod::VERB | KeywordMod::PAST_PARTICIPLE))
            });

            let left = TokenStream::from_str(&gerund).unwrap();
            first.extend(quote! {
                ,#left #fat Some(#output::from_span_start(self.make_position(), kw_slice.len(), KeywordMod::VERB | KeywordMod::GERUND))
            });

            first
        }
        MatchArm::Other { other, ident, fat, output } => {
            let arr = ident_to_array(ident.value(), None);
            let let_token = syn::token::Let {
                span: other.span,
            };

            let left = TokenStream::from_str(&arr).unwrap();
            let first = quote! {
                #left #fat {
                    #let_token mods = KeywordMod::empty();
                    Some(#output::from_span_start(self.make_position(), kw_slice.len(), mods))
                }
            };

            first
        }
        MatchArm::Identity { arm } => arm.to_token_stream(),
    });

    let output = quote! {
        #match_tok #ident {
            #(#arms),*
        }
    };

    output.into_token_stream().into()
}

fn derive_ast_node_helper(
    ident: &syn::Ident,
    fields: &Fields,
    enum_field: Option<&syn::Ident>,
    (ignore_items, ignore_types): &(HashSet<String>, HashSet<Type>),
) -> TokenStream {
    let mut output = TokenStream::new();
    match &fields {
        Fields::Unnamed(fields) => {
            let mut generate =
                |left: i32, mut right: i32, left_name: &TokenStream, stream: &TokenStream| {
                    while left <= right && right >= 0 {
                        let right_field = &fields.unnamed[right as usize];

                        if has_attr(&right_field.attrs, &["skip_item"]).is_some()
                            || ignore_types.contains(&right_field.ty)
                        {
                            right -= 1;
                            continue;
                        }

                        let right_str = format!("_a{}", right);
                        let right_name = TokenStream::from_str(&right_str).unwrap();

                        if ignore_items.contains(&right_str) {
                            right -= 1;
                            continue;
                        }

                        let ignore_rights: String = (right as usize..fields.unnamed.len() - 1)
                            .map(|_| "_")
                            .collect::<Vec<_>>()
                            .join(",");

                        let mut out = stream.clone();

                        if left != right {
                            if is_optional(&fields.unnamed[right as usize].ty) {
                                out.extend(quote::quote! { Some(#right_name) });
                            } else {
                                out.extend(right_name.clone());
                            }
                        }

                        if !ignore_rights.is_empty() {
                            if left != right {
                                out.extend(quote::quote!(,));
                            }
                            out.extend(TokenStream::from_str(&ignore_rights));
                        }

                        if let Some(enum_field) = enum_field {
                            output.extend(quote::quote! {
                                #ident::#enum_field (#out) => (&#left_name, &#right_name).as_span(),
                            });
                        } else {
                            output.extend(quote::quote! {
                                #ident (#out) => (&#left_name, &#right_name).as_span(),
                            });
                        }

                        if is_optional(&fields.unnamed[right as usize].ty) {
                            right -= 1
                        } else {
                            break;
                        }
                    }
                };

            let mut left = 0;
            let right = fields.unnamed.len() as i32 - 1;

            while left < fields.unnamed.len() as i32 {
                let left_field = &fields.unnamed[left as usize];

                if has_attr(&left_field.attrs, &["skip_item"]).is_some()
                    || ignore_types.contains(&left_field.ty)
                {
                    left += 1;
                    continue;
                }

                let left_str = format!("_a{}", left);
                let left_name = TokenStream::from_str(&left_str).unwrap();

                if ignore_items.contains(&left_str) {
                    left -= 1;
                    continue;
                }

                let mut inner_names = TokenStream::new();
                let ignore_lefts: String = (0..left as usize)
                    .map(|_| "_")
                    .collect::<Vec<_>>()
                    .join(",");

                if !ignore_lefts.is_empty() {
                    inner_names.extend(TokenStream::from_str(&ignore_lefts));
                    inner_names.extend(quote::quote!(,));
                }

                if is_optional(&left_field.ty) {
                    inner_names.extend(quote::quote! { Some(#left_name) });
                } else {
                    inner_names.extend(left_name.clone());
                }

                inner_names.extend(quote::quote!(,..,));

                generate(left, right, &left_name, &inner_names);

                if is_optional(&left_field.ty) {
                    left += 1;
                } else {
                    break;
                }
            }
        }
        Fields::Unit => {
            if let Some(enum_field) = enum_field {
                output.extend(quote::quote! {
                    #ident::#enum_field => Span::default(),
                });
            } else {
                output.extend(quote::quote! {
                    #ident => Span::default(),
                });
            }
        }
        Fields::Named(fields) => {
            let mut generate = |left: i32,
                                mut right: i32,
                                left_name: &TokenStream,
                                stream: &TokenStream| {
                while left <= right && right >= 0 {
                    let right_field = &fields.named[right as usize];

                    if has_attr(&right_field.attrs, &["skip_item"]).is_some()
                        || ignore_types.contains(&right_field.ty)
                        || ignore_items.contains(&right_field.ident.as_ref().unwrap().to_string())
                    {
                        right -= 1;
                        continue;
                    }

                    let right_name = right_field.ident.clone().unwrap().to_token_stream();

                    let mut out = stream.clone();

                    if left != right {
                        if is_optional(&right_field.ty) {
                            out.extend(quote::quote! { #right_name: Some(#right_name) });
                        } else {
                            out.extend(right_name.clone());
                        }
                        out.extend(quote::quote! {,});
                    }

                    if let Some(enum_field) = enum_field {
                        output.extend(quote::quote! {
                                        #ident::#enum_field { #out .. } => (&#left_name, &#right_name).as_span(),
                                    });
                    } else {
                        output.extend(quote::quote! {
                            #ident { #out .. } => (&#left_name, &#right_name).as_span(),
                        });
                    }

                    if is_optional(&right_field.ty) {
                        right -= 1
                    } else {
                        break;
                    }
                }
            };

            let mut left = 0;
            let right = fields.named.len() as i32 - 1;

            while left < fields.named.len() as i32 {
                let left_field = &fields.named[left as usize];

                if has_attr(&left_field.attrs, &["skip_item"]).is_some()
                    || ignore_types.contains(&left_field.ty)
                    || ignore_items.contains(&left_field.ident.as_ref().unwrap().to_string())
                {
                    left += 1;
                    continue;
                }

                let left_name = left_field.ident.clone().unwrap().to_token_stream();

                let mut inner_names = TokenStream::new();

                if is_optional(&left_field.ty) {
                    inner_names.extend(quote::quote! { #left_name: Some(#left_name) });
                } else {
                    inner_names.extend(left_name.clone());
                }

                inner_names.extend(quote::quote! {,});

                generate(left, right, &left_name, &inner_names);

                if is_optional(&left_field.ty) {
                    left += 1;
                } else {
                    break;
                }
            }
        }
    };

    output
}

#[proc_macro_derive(AsSpan, attributes(skip_item, skip_all))]
pub fn derive_ast_node(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    let ident = input.ident;

    let ignore = has_attr(&input.attrs, &["skip_all"])
        .map(attr_args)
        .unwrap_or((HashSet::new(), HashSet::new()));

    let params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let struct_args = params.iter().map(|param| match param {
        GenericParam::Type(tp) => tp.ident.to_token_stream(),
        GenericParam::Const(tp) => tp.ident.to_token_stream(),
        GenericParam::Lifetime(tp) => tp.lifetime.to_token_stream(),
    });

    let mut num_childs = TokenStream::new();

    match &input.data {
        Data::Struct(node) => {
            num_childs.extend(derive_ast_node_helper(&ident, &node.fields, None, &ignore));
        }
        Data::Enum(node) => {
            for variant in &node.variants {
                let name = &variant.ident;
                num_childs.extend(derive_ast_node_helper(
                    &ident,
                    &variant.fields,
                    Some(name),
                    &ignore,
                ));
            }
        }
        _ => panic!("Unexpected type"),
    };

    let tokens = quote::quote! {
        impl <#params> parsely_lexer::AsSpan for #ident<#(#struct_args),*> #where_clause {
            fn as_span(&self) -> parsely_lexer::Span {
                match self {
                    #num_childs
                }
            }
        }
    };

    tokens.into()
}
