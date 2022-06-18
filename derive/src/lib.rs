//! This crate defines a procedural derive macro for implementing the `TypeDef`
//! trait from the `typescript_type_def` crate.
//!
//! See the documentation of that crate for more information.
#![warn(rust_2018_idioms, clippy::all)]
#![deny(clippy::correctness)]
#![allow(clippy::match_like_matches_macro)]

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use darling::FromDeriveInput;
use export::{
    export_type_infos, type_infos_expr, ConstExport, ConstExportArgs,
    ExportToFile, ExportedTypeInfos,
};
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use quote::quote;
use syn::parse_macro_input;

mod export;
mod type_def;

#[proc_macro_error]
#[proc_macro_derive(TypeDef, attributes(type_def, serde))]
pub fn derive_type_def(input: TokenStream) -> TokenStream {
    let input = match syn::parse2::<syn::DeriveInput>(input.into()) {
        Ok(data) => data,
        Err(err) => return err.to_compile_error().into(),
    };
    let mut input = match type_def::TypeDefInput::from_derive_input(&input) {
        Ok(input) => input,
        Err(error) => return error.write_errors().into(),
    };

    type_def::derive_type_def(&mut input).into()
}

#[proc_macro_attribute]
pub fn export(attr: TokenStream, item: TokenStream) -> TokenStream {
    let const_export_args = parse_macro_input!(attr as ConstExportArgs);
    let const_export = parse_macro_input!(item as ConstExport);
    let hash = hash((&const_export_args, &const_export));
    let ConstExportArgs { types } = const_export_args;

    let ExportedTypeInfos {
        export_tokens,
        path,
    } = export_type_infos(hash, type_infos_expr(types), None);
    let contents = std::fs::read_to_string(path.unwrap()).unwrap();
    let const_decl = const_export.with_literal_string(&contents);

    quote! {
        #export_tokens
        #const_decl
    }
    .into()
}

#[cfg(feature = "export-all")]
#[proc_macro_attribute]
pub fn export_all(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let const_export = parse_macro_input!(item as ConstExport);
    let ExportedTypeInfos {
        export_tokens,
        path,
    } = export_type_infos("all", export::export_all_type_infos_expr(), None);
    let contents = std::fs::read_to_string(path.unwrap()).unwrap();
    let const_decl = const_export.with_literal_string(&contents);
    let export_all_slice_decl = export::export_all_slice_decl();

    quote! {
        #export_all_slice_decl
        #export_tokens
        #const_decl
    }
    .into()
}

#[proc_macro]
pub fn export_to_file(input: TokenStream) -> TokenStream {
    let export_to_file = parse_macro_input!(input as ExportToFile);
    let hash = hash(&export_to_file);
    let ExportToFile { types, file, .. } = export_to_file;

    let ExportedTypeInfos {
        export_tokens,
        path: _,
    } = export_type_infos(hash, type_infos_expr(types), Some(file));

    quote! {
        #export_tokens
    }
    .into()
}

#[cfg(feature = "export-all")]
#[proc_macro]
pub fn export_all_to_file(input: TokenStream) -> TokenStream {
    let export::ExportAllToFile { file } =
        parse_macro_input!(input as export::ExportAllToFile);

    let ExportedTypeInfos {
        export_tokens,
        path: _,
    } = export_type_infos(
        "all",
        export::export_all_type_infos_expr(),
        Some(file),
    );
    let export_all_slice_decl = export::export_all_slice_decl();

    quote! {
        #export_all_slice_decl
        #export_tokens
    }
    .into()
}

fn hash(h: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    h.hash(&mut hasher);
    hasher.finish()
}
