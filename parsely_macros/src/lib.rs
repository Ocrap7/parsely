use std::{collections::HashSet, str::FromStr};

use proc_macro::TokenTree;
use proc_macro2::TokenStream;
use quote::ToTokens;
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

#[proc_macro]
pub fn consume_kw(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut iter = tokens.into_iter();

    let Some(TokenTree::Ident(ident)) = iter.next() else {
        panic!("Expected ident!")
    };

    let fstr = format!("Some({}::from_span_start(self.make_position()))", ident);

    proc_macro::TokenStream::from_str(&fstr).unwrap()
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
