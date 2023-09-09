use std::str::FromStr;

use proc_macro::{TokenStream, TokenTree};

fn ident_to_array<I: ToString>(ident: I, append: Option<&str>) -> String {
    let mut lit_string = ident.to_string();
    if let Some(append) = append {
        lit_string.push_str(append);
    }

    let arr = lit_string
        .chars()
        .skip(1)
        .take(lit_string.len() - 2)
        .map(|l| l.to_string())
        .collect::<Vec<_>>()
        .join("', '");

    format!("['{}']", arr)
}

#[proc_macro]
pub fn match_token(input: TokenStream) -> TokenStream {
    let mut iter = input.into_iter();

    let Some(TokenTree::Literal(lit)) = iter.next() else {
        panic!("Expected string!")
    };

    let str_val = ident_to_array(lit, None);

    TokenStream::from_str(&str_val).unwrap()
}

#[proc_macro]
pub fn consume_kw(tokens: TokenStream) -> proc_macro::TokenStream {
    let mut iter = tokens.into_iter();

    let Some(TokenTree::Ident(ident)) = iter.next() else {
        panic!("Expected ident!")
    };

    let fstr = format!("Some({}::from_span_start(self.make_position()))", ident);

    TokenStream::from_str(&fstr).unwrap()
}
