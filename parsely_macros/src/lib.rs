use std::str::FromStr;

use proc_macro::{TokenStream, TokenTree};

#[proc_macro]
pub fn str_arr(tokens: TokenStream) -> TokenStream {
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

    let toks = TokenStream::from_str(&fstr).unwrap();

    toks
}

#[proc_macro]
pub fn consume_kw(tokens: TokenStream) -> TokenStream {
    let mut iter = tokens.into_iter();

    let Some(TokenTree::Ident(ident)) = iter.next() else {
        panic!("Expected ident!")
    };

    let fstr = format!("Some({}::from_span_start(self.make_position()))", ident);

    let toks = TokenStream::from_str(&fstr).unwrap();

    toks
}