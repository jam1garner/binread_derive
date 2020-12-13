mod parser;
mod spanned_value;
mod top_level_attrs;
mod field_level_attrs;
pub(crate) use top_level_attrs::TopLevelAttrs;
pub(crate) use field_level_attrs::FieldLevelAttrs;
pub(crate) use spanned_value::SpannedValue;

use proc_macro2::TokenStream;
use crate::compiler_error::{CompileError, SpanError};
use syn::{Expr, Field, Ident, Lit, Meta, NestedMeta, parse::Parse, Path, Type, spanned::Spanned};
use quote::ToTokens;
use std::str::FromStr;
use syn::export::TokenStream2;

use self::parser::MetaList;

#[derive(Debug, Clone)]
pub struct Assert(pub TokenStream, pub Option<TokenStream>);

#[derive(Debug)]
struct MultiformExpr(TokenStream);

#[derive(Debug, Default, Clone)]
pub struct PassedValues(Vec<TokenStream>);

#[derive(Debug, Clone)]
pub enum PassedArgs {
    List(PassedValues),
    Tuple(TokenStream)
}

impl Default for PassedArgs {
    fn default() -> Self {
        PassedArgs::List(PassedValues::default())
    }
}

impl PassedValues {
    pub fn iter(&self) -> impl Iterator<Item = &TokenStream> {
        self.0.iter()
    }
}

#[derive(Debug, Clone)]
pub enum Imports {
    List(Vec<Ident>, Vec<Type>),
    Tuple(Ident, Box<Type>)
}

impl Default for Imports {
    fn default() -> Self {
        Imports::List(Vec::new(), Vec::new())
    }
}

impl Imports {
    pub fn idents(&self) -> TokenStream2 {
        match self {
            Imports::List(idents, _) => {
                let idents = idents.iter();
                quote::quote! {
                    (#(mut #idents,)*)
                }
            },
            Imports::Tuple(ident, _) => quote::quote! {
                mut #ident
            }
        }
    }

    pub fn types(&self) -> TokenStream2 {
        match self {
            Imports::List(_, types) => {
                let types = types.iter();
                quote::quote! {
                    (#(#types,)*)
                }
            },
            Imports::Tuple(_, ty) => {
                ty.to_token_stream()
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Imports::List(idents, _) => idents.is_empty(),
            Imports::Tuple(_, _) => false
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum MagicType {
    Str,
    ByteStr,
    Byte,
    Char,
    Int(String),
    Float,
    Bool,
    Verbatim
}

fn convert_assert<K>(assert: &MetaList<K, Expr>) -> Result<Assert, CompileError>
    where K: Parse + Spanned,
{
    let (cond, err) = match assert.fields[..] {
        [ref cond] => {
            (cond, None)
        }
        [ref cond, ref err] => {
            (cond, Some(err))
        }
        _ => return SpanError::err(
            assert.ident.span(),
            ""
        ).map_err(Into::into),
    };

    Ok(Assert(
        cond.into_token_stream(),
        err.map(ToTokens::into_token_stream)
    ))
}
