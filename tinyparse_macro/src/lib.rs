use syn::{Expr, parse::Parse, punctuated::Punctuated, Token, parse_macro_input, Generics, GenericParam, parse_quote, spanned::Spanned, TypeParam};
use quote::{quote, quote_spanned, format_ident};

struct Args {
    parsers: Vec<Expr>,
}

impl Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let exprs = Punctuated::<Expr, Token![,]>::parse_terminated(input)?;
        Ok(Args {
            parsers: exprs.into_iter().collect(),
        })
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(input as Args);

    let mut generics = Generics::default();
    generics.params.push(GenericParam::Lifetime(parse_quote!('a)));
    for i in 0..args.parsers.len() { 
        let ident = format_ident!("R{}", i);
        generics.params.push(GenericParam::Type(parse_quote!(#ident)));
    }
    
    
    let (impl_generics, _, _) = generics.split_for_impl();

    let args_decl = args.parsers.iter().enumerate().map(|(n, parser)| {
        let var_name = format_ident!("p{}", n);
        let var_generic = format_ident!("R{}", n);
        quote_spanned! {parser.span()=>
            #var_name: impl Parse<'a, #var_generic>
        }
    });

    let tup_type_insides = (0..args.parsers.len()).map(|i| {
        let ident = format_ident!("R{}", i);
        ident
    });

    let parse_one = args.parsers.iter().enumerate().map(|(n, parser)| {
        let res_name = format_ident!("r{}", n);
        let parser_name = format_ident!("p{}", n);
        quote_spanned! {parser.span()=>
            let (span, #res_name) = #parser_name.parse(span)?;
        }
    });

    let typ_return = args.parsers.iter().enumerate().map(|(n, _)| {
        format_ident!("r{}", n)
    });

    let call_expr = args.parsers.iter();
    let expanded_fn = quote! {
        {
            fn __seq_impl #impl_generics(#(#args_decl),*) -> impl Parse<'a, (#(#tup_type_insides),*)> {
                move |span: Span<'a>| {
                    #(#parse_one)*

                    Ok((span, (#(#typ_return),*)))
                }
            }
            __seq_impl(#(#call_expr),*)
        }
    };

    proc_macro::TokenStream::from(expanded_fn)
}