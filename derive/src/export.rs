use std::{fmt::Display, path::PathBuf};

use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::Punctuated,
    Attribute, Expr, Ident, Result, Token, Type, Visibility,
};

#[derive(Hash)]
pub struct ConstExportArgs {
    pub types: Punctuated<Type, Token![,]>,
}

impl Parse for ConstExportArgs {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(ConstExportArgs {
            types: input.parse_terminated(Type::parse)?,
        })
    }
}

#[derive(Hash)]
pub struct ConstExport {
    pub attrs: Vec<Attribute>,
    pub visibility: Visibility,
    pub const_token: Token![const],
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Type,
    pub semicolon_token: Token![;],
}

impl Parse for ConstExport {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(ConstExport {
            attrs: input.call(Attribute::parse_outer)?,
            visibility: input.parse()?,
            const_token: input.parse()?,
            ident: input.parse()?,
            colon_token: input.parse()?,
            ty: input.parse()?,
            semicolon_token: input.parse()?,
        })
    }
}

impl ConstExport {
    pub fn with_literal_string(&self, contents: &str) -> TokenStream {
        let ConstExport {
            attrs,
            visibility,
            const_token,
            ident,
            colon_token,
            ty,
            semicolon_token,
        } = self;
        let literal = Literal::string(contents);

        quote! {
            #(#attrs)*
            #visibility #const_token #ident #colon_token #ty = #literal #semicolon_token
        }
    }
}

#[derive(Hash)]
pub struct ExportToFile {
    pub file: Expr,
    pub semicolon_token: Token![;],
    pub types: Punctuated<Type, Token![,]>,
}

impl Parse for ExportToFile {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(ExportToFile {
            file: input.parse()?,
            semicolon_token: input.parse()?,
            types: input.parse_terminated(Type::parse)?,
        })
    }
}

#[cfg(feature = "export-all")]
#[derive(Hash)]
pub struct ExportAllToFile {
    pub file: Expr,
}

#[cfg(feature = "export-all")]
impl Parse for ExportAllToFile {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(ExportAllToFile {
            file: input.parse()?,
        })
    }
}

fn get_type_infos_path(id: &impl Display) -> PathBuf {
    let mut type_infos_path = PathBuf::from(env!("OUT_DIR"));
    type_infos_path.push(format!("{id}.d.ts"));
    type_infos_path
}

pub struct ExportedTypeInfos {
    pub export_tokens: TokenStream,
    /// Path assigned to the file which contains the output. `None` if the
    /// input `type_infos_path` is `Some()`.
    pub path: Option<PathBuf>,
}

pub fn export_type_infos(
    id: impl Display,
    type_infos_expr: Expr,
    type_infos_path: Option<Expr>,
) -> ExportedTypeInfos {
    let (type_infos_path, type_infos_path_expr) =
        if let Some(expr) = type_infos_path {
            (None, expr)
        } else {
            let type_infos_path = get_type_infos_path(&id);
            if !type_infos_path.exists() {
                // Make sure that the file exists, since `include_str!()` will fail
                // if the file does not exist.
                std::fs::File::options()
                    .create(true)
                    .append(true)
                    .open(&type_infos_path)
                    .unwrap();
            }
            let type_infos_path_lit = Literal::string(
                type_infos_path
                    .to_str()
                    .expect("cannot convert out path to UTF-8"),
            );

            (Some(type_infos_path), parse_quote! { #type_infos_path_lit })
        };
    // We compare `include_str!()` to the read value when using a literal,
    // because the call to `include_str!()` is supposed to ensure the binary
    // will reload when the pointed-at file changes, which is necessary for this
    // test to work correctly.
    let include_type_infos_path_stmt = type_infos_path.is_some().then(|| {
        quote! {
            assert_eq!(include_str!(#type_infos_path_expr), actual);
        }
    });
    let test_name = format_ident!("ensure_type_infos_are_up_to_date_{id}");

    let export_tokens = quote! {
        #[test]
        fn #test_name() {
            let actual_path = ::typescript_type_def::macro_support::join_to_given_path(
                env!("CARGO_MANIFEST_DIR"), file!(), #type_infos_path_expr);
            let actual = ::std::fs::read_to_string(&actual_path).unwrap_or_default();
            let expected =
                ::typescript_type_def::macro_support::type_infos_to_string(#type_infos_expr);

            #include_type_infos_path_stmt

            if actual != expected {
                ::std::fs::write(&actual_path, expected).unwrap();
            }
        }
    };

    ExportedTypeInfos {
        export_tokens,
        path: type_infos_path,
    }
}

pub fn type_infos_expr(types: impl IntoIterator<Item = Type>) -> Expr {
    let types = types.into_iter();

    parse_quote! {{
        use ::typescript_type_def::TypeDef;

        &[#(#types::GET_INFO_FN()),*]
    }}
}

#[cfg(feature = "export-all")]
pub fn export_all_slice_decl() -> TokenStream {
    quote! {
        #[cfg(test)]
        #[::typescript_type_def::macro_support::linkme::distributed_slice]
        #[linkme(crate = ::typescript_type_def::macro_support::linkme)]
        pub(crate) static TYPE_INFOS: [&'static ::typescript_type_def::type_expr::TypeInfo] =
            [..];
    }
}

#[cfg(feature = "export-all")]
pub fn export_all_type_infos_expr() -> Expr {
    parse_quote! {
        &crate::TYPE_INFOS[..]
    }
}
