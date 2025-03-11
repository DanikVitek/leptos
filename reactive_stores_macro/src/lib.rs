use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream};
use proc_macro_error2::{abort, abort_call_site, proc_macro_error, OptionExt};
use quote::{quote, ToTokens};
use std::borrow::Cow;
use syn::{
    parse::{Parse, ParseStream, Parser},
    punctuated::Punctuated,
    spanned::Spanned,
    token::Comma,
    ExprClosure, Field, Fields, Generics, Ident, Index, Meta, Result, Token,
    Type, Variant, Visibility, WhereClause,
};

#[proc_macro_error]
#[proc_macro_derive(Store, attributes(store))]
pub fn derive_store(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse_macro_input!(input as Model)
        .into_token_stream()
        .into()
}

#[proc_macro_error]
#[proc_macro_derive(Patch, attributes(store, patch))]
pub fn derive_patch(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syn::parse_macro_input!(input as PatchModel)
        .into_token_stream()
        .into()
}

struct Model {
    vis: Visibility,
    name: Ident,
    generics: Generics,
    ty: ModelTy,
}

enum ModelTy {
    Struct { fields: Vec<Field> },
    Enum { variants: Vec<Variant> },
}

impl Parse for Model {
    fn parse(input: ParseStream) -> Result<Self> {
        let input = syn::DeriveInput::parse(input)?;

        let ty = match input.data {
            syn::Data::Struct(s) => ModelTy::Struct {
                fields: collect_struct_fields(s.fields, s.semi_token.span()),
            },
            syn::Data::Enum(e) => ModelTy::Enum {
                variants: e.variants.into_iter().collect(),
            },
            _ => {
                abort_call_site!(
                    "only structs and enums can be used with `Store`"
                );
            }
        };

        Ok(Self {
            vis: input.vis,
            generics: input.generics,
            name: input.ident,
            ty,
        })
    }
}

#[derive(Clone)]
enum SubfieldMode {
    Keyed(ExprClosure, Box<Type>),
    Skip,
}

impl Parse for SubfieldMode {
    fn parse(input: ParseStream) -> Result<Self> {
        let mode: Ident = input.parse()?;
        if mode == "key" {
            let _col: Token![:] = input.parse()?;
            let ty: Type = input.parse()?;
            let _eq: Token![=] = input.parse()?;
            let closure: ExprClosure = input.parse()?;
            Ok(SubfieldMode::Keyed(closure, Box::new(ty)))
        } else if mode == "skip" {
            Ok(SubfieldMode::Skip)
        } else {
            Err(input.error("expected `key: <Type> = <closure>`"))
        }
    }
}

impl ToTokens for Model {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let library_path = quote! { reactive_stores };
        let Model {
            vis,
            name,
            generics,
            ty,
        } = &self;
        let any_store_field = Ident::new("AnyStoreField", Span::call_site());
        let trait_name = Ident::new(&format!("{name}StoreFields"), name.span());
        let generics_with_orig = {
            let params = &generics.params;
            quote! { <#any_store_field, #params> }
        };
        let where_with_orig = {
            generics
                .where_clause
                .as_ref()
                .map(|w| {
                    let WhereClause {
                        where_token,
                        predicates,
                    } = &w;
                    quote! {
                        #where_token
                            #any_store_field: #library_path::StoreField<Value = #name #generics>,
                            #predicates
                    }
                })
                .unwrap_or_else(|| quote! { where #any_store_field: #library_path::StoreField<Value = #name #generics> })
        };

        // define an extension trait that matches this struct
        // and implement that trait for all StoreFields
        let (trait_fields, read_fields): (Vec<_>, Vec<_>) =
            ty.to_field_data(&library_path, generics, &any_store_field, name);

        // read access
        tokens.extend(quote! {
            #vis trait #trait_name <AnyStoreField>
            #where_with_orig
            {
                #(#trait_fields)*
            }

            #[automatically_derived]
            impl #generics_with_orig #trait_name <AnyStoreField> for AnyStoreField
            #where_with_orig
            {
               #(#read_fields)*
            }
        });
    }
}

impl ModelTy {
    fn to_field_data(
        &self,
        library_path: &TokenStream,
        generics: &Generics,
        any_store_field: &Ident,
        name: &Ident,
    ) -> (Vec<TokenStream>, Vec<TokenStream>) {
        match self {
            ModelTy::Struct { fields } => fields
                .iter()
                .enumerate()
                .map(|(idx, field)| {
                    let Field {
                        ident, ty, attrs, ..
                    } = &field;
                    let modes = attrs
                        .iter()
                        .find_map(|attr| {
                            attr.meta.path().is_ident("store").then(|| {
                                match &attr.meta {
                                    Meta::List(list) => {
                                        match Punctuated::<
                                                SubfieldMode,
                                                Comma,
                                            >::parse_terminated
                                                .parse2(list.tokens.clone())
                                            {
                                                Ok(modes) => Some(
                                                    modes
                                                        .into_iter()
                                                        .collect::<Vec<_>>(),
                                                ),
                                                Err(e) => abort!(list, e),
                                            }
                                    }
                                    _ => None,
                                }
                            })
                        })
                        .flatten();

                    (
                        field_to_tokens::<false>(
                            idx,
                            modes.as_deref(),
                            library_path,
                            ident.as_ref(),
                            generics,
                            any_store_field,
                            name,
                            ty,
                        ),
                        field_to_tokens::<true>(
                            idx,
                            modes.as_deref(),
                            library_path,
                            ident.as_ref(),
                            generics,
                            any_store_field,
                            name,
                            ty,
                        ),
                    )
                })
                .unzip(),
            ModelTy::Enum { variants } => variants
                .iter()
                .map(|variant| {
                    let Variant {
                        ident: variant_name,
                        fields,
                        ..
                    } = variant;

                    (
                        variant_to_tokens::<false>(
                            library_path,
                            variant_name,
                            generics,
                            any_store_field,
                            name,
                            fields,
                        ),
                        variant_to_tokens::<true>(
                            library_path,
                            variant_name,
                            generics,
                            any_store_field,
                            name,
                            fields,
                        ),
                    )
                })
                .unzip(),
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn field_to_tokens<const INCLUDE_BODY: bool>(
    field_idx: usize,
    modes: Option<&[SubfieldMode]>,
    library_path: &TokenStream,
    field_name: Option<&Ident>,
    generics: &Generics,
    any_store_field: &Ident,
    type_name: &Ident,
    ty: &Type,
) -> TokenStream {
    let (fn_name, locator) = match field_name {
        None => (
            Cow::Owned(Ident::new(
                &format!("field{field_idx}"),
                Span::call_site(),
            )),
            Either::Right(Index::from(field_idx)),
        ),
        Some(field_name) => {
            (Cow::Borrowed(field_name), Either::Left(field_name))
        }
    };

    if let Some(modes) = modes {
        if let [mode] = modes {
            return match mode {
                SubfieldMode::Keyed(keyed_by, key_ty) => {
                    if INCLUDE_BODY {
                        quote! {
                            fn #fn_name(self) ->  #library_path::KeyedSubfield<#any_store_field, #type_name #generics, #key_ty, #ty> {
                                #library_path::KeyedSubfield::new(
                                    self,
                                    #field_idx.into(),
                                    #keyed_by,
                                    |prev| &prev.#locator,
                                    |prev| &mut prev.#locator,
                                )
                            }
                        }
                    } else {
                        quote! {
                            fn #fn_name(self) ->  #library_path::KeyedSubfield<#any_store_field, #type_name #generics, #key_ty, #ty>;
                        }
                    }
                }
                SubfieldMode::Skip => quote! {},
            };
        } else {
            abort!(
                field_name
                    .map(|ident| ident.span())
                    .unwrap_or_else(Span::call_site),
                "multiple modes not currently supported"
            );
        }
    }

    // default subfield
    if INCLUDE_BODY {
        quote! {
            fn #fn_name(self) ->  #library_path::Subfield<#any_store_field, #type_name #generics, #ty> {
                #library_path::Subfield::new(
                    self,
                    #field_idx.into(),
                    |prev| &prev.#locator,
                    |prev| &mut prev.#locator,
                )
            }
        }
    } else {
        quote! {
            fn #fn_name(self) ->  #library_path::Subfield<#any_store_field, #type_name #generics, #ty>;
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn variant_to_tokens<const INCLUDE_BODY: bool>(
    library_path: &TokenStream,
    variant_name: &Ident,
    generics: &Generics,
    any_store_field: &Ident,
    enum_name: &Ident,
    fields: &Fields,
) -> TokenStream {
    // the method name will always be the snake_cased ident
    let fn_name = Ident::new(
        &variant_name.to_string().to_case(Case::Snake),
        variant_name.span(),
    );

    match fields {
        // For unit enum fields, we will just return a `bool` subfield, which is
        // true when this field matches
        Fields::Unit => {
            // default subfield
            make_is_variant::<INCLUDE_BODY>(
                enum_name,
                &fn_name,
                variant_name,
                library_path,
            )
        }
        // If an enum branch has named fields, we create N + 1 methods:
        // 1 `bool` subfield, which is true when this field matches
        // N `Option<T>` subfields for each of the named fields
        Fields::Named(fields) => {
            let mut tokens = make_is_variant::<INCLUDE_BODY>(
                enum_name,
                &fn_name,
                variant_name,
                library_path,
            );

            tokens.extend(fields
                .named
                .iter()
                .map(|field| {
                    let field_ident = field.ident.as_ref().unwrap();
                    let field_ty = &field.ty;
                    let combined_ident = Ident::new(
                        &format!("{}_{}", fn_name, field_ident),
                        field_ident.span(),
                    );

                    // default subfield
                    if !INCLUDE_BODY {
                        return quote! {
                            fn #combined_ident(self) -> Option<#library_path::Subfield<#any_store_field, #enum_name #generics, #field_ty>>;
                        };
                    }
                    quote! {
                        fn #combined_ident(self) -> Option<#library_path::Subfield<#any_store_field, #enum_name #generics, #field_ty>> {
                            #library_path::StoreField::track_field(&self);
                            let reader = #library_path::StoreField::reader(&self);
                            let matches = reader
                                .map(|reader| matches!(&*reader, #enum_name::#variant_name { .. }))
                                .unwrap_or(false);
                            if !matches {
                                return None;
                            }
                            Some(#library_path::Subfield::new(
                                self,
                                0.into(),
                                |prev| {
                                    match prev {
                                        #enum_name::#variant_name { #field_ident, .. } => Some(#field_ident),
                                        _ => None,
                                    }
                                    .expect("accessed an enum field that is no longer matched")
                                },
                                |prev| {
                                    match prev {
                                        #enum_name::#variant_name { #field_ident, .. } => Some(#field_ident),
                                        _ => None,
                                    }
                                    .expect("accessed an enum field that is no longer matched")
                                },
                            ))
                        }
                    }
                }));

            tokens
        }
        // If an enum branch has unnamed fields, we create N + 1 methods:
        // 1 `bool` subfield, which is true when this field matches
        // N `Option<T>` subfields for each of the unnamed fields
        Fields::Unnamed(fields) => {
            let mut tokens = make_is_variant::<INCLUDE_BODY>(
                enum_name,
                &fn_name,
                variant_name,
                library_path,
            );

            let number_of_fields = fields.unnamed.len();

            tokens.extend(fields
                .unnamed
                .iter()
                .enumerate()
                .map(|(idx, field)| {
                    let field_ident = idx;
                    let field_ty = &field.ty;
                    let combined_ident = Ident::new(
                        &format!("{}_{}", fn_name, field_ident),
                        fn_name.span(),
                    );

                    let ignore_before = (0..idx).map(|_| quote! { _, });
                    let ignore_before2 = ignore_before.clone();
                    let ignore_after = (idx..number_of_fields.saturating_sub(1)).map(|_| quote !{_, });
                    let ignore_after2 = ignore_after.clone();

                    // default subfield
                    if !INCLUDE_BODY {
                        return quote! {
                            fn #combined_ident(self) -> Option<#library_path::Subfield<#any_store_field, #enum_name #generics, #field_ty>>;
                        };
                    }
                    quote! {
                        fn #combined_ident(self) -> Option<#library_path::Subfield<#any_store_field, #enum_name #generics, #field_ty>> {
                            #library_path::StoreField::track_field(&self);
                            let reader = #library_path::StoreField::reader(&self);
                            let matches = reader
                                .map(|reader| matches!(&*reader, #enum_name::#variant_name(..)))
                                .unwrap_or(false);
                            if !matches {
                                return None;
                            }
                            Some(#library_path::Subfield::new(
                                self,
                                0.into(),
                                |prev| {
                                    match prev {
                                        #enum_name::#variant_name(#(#ignore_before)* this, #(#ignore_after)*) => Some(this),
                                        _ => None,
                                    }
                                    .expect("accessed an enum field that is no longer matched")
                                },
                                |prev| {
                                    match prev {
                                        #enum_name::#variant_name(#(#ignore_before2)* this, #(#ignore_after2)*) => Some(this),
                                        _ => None,
                                    }
                                    .expect("accessed an enum field that is no longer matched")
                                },
                            ))
                        }
                    }
                }));

            tokens
        }
    }
}

fn make_is_variant<const INCLUDE_BODY: bool>(
    enum_name: &Ident,
    fn_name: &Ident,
    variant_name: &Ident,
    library_path: &TokenStream,
) -> TokenStream {
    if !INCLUDE_BODY {
        return quote! { fn #fn_name(self) -> bool; };
    }
    quote! {
        fn #fn_name(self) -> bool {
            match #library_path::StoreField::reader(&self) {
                Some(reader) => {
                    #library_path::StoreField::track_field(&self);
                    matches!(&*reader, #enum_name::#variant_name)
                },
                None => false
            }
        }
    }
}

struct PatchModel {
    pub name: Ident,
    pub generics: Generics,
    pub ty: PatchModelTy,
}

enum PatchModelTy {
    Struct {
        fields: Vec<Field>,
    },
    #[allow(dead_code)]
    Enum {
        variants: Vec<Variant>,
    },
}

impl Parse for PatchModel {
    fn parse(input: ParseStream) -> Result<Self> {
        let input = syn::DeriveInput::parse(input)?;

        let ty = match input.data {
            syn::Data::Struct(s) => {
                let fields = match s.fields {
                    Fields::Unit => {
                        abort!(s.semi_token, "unit structs are not supported");
                    }
                    Fields::Named(fields) => {
                        fields.named.into_iter().collect::<Vec<_>>()
                    }
                    Fields::Unnamed(fields) => {
                        fields.unnamed.into_iter().collect::<Vec<_>>()
                    }
                };

                PatchModelTy::Struct { fields }
            }
            syn::Data::Enum(_e) => {
                abort_call_site!("only structs can be used with `Patch`");

                // TODO: support enums later on
                // PatchModelTy::Enum {
                //     variants: e.variants.into_iter().collect(),
                // }
            }
            _ => {
                abort_call_site!(
                    "only structs and enums can be used with `Store`"
                );
            }
        };

        Ok(Self {
            name: input.ident,
            generics: input.generics,
            ty,
        })
    }
}

fn collect_struct_fields<T>(fields: Fields, abort_span: Span) -> T
where
    T: FromIterator<Field>,
{
    match fields {
        Fields::Unit => abort!(abort_span, "unit structs are not supported"),
        Fields::Named(fields) => fields.named.into_iter(),
        Fields::Unnamed(fields) => fields.unnamed.into_iter(),
    }
    .collect()
}

impl ToTokens for PatchModel {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let library_path = quote! { reactive_stores };
        let PatchModel { name, generics, ty } = &self;

        let fields = match ty {
            PatchModelTy::Struct { fields } => {
                fields.iter().enumerate().map(|(idx, field)| {
                    let Field {
                        attrs, ident, ..
                    } = &field;
                    let locator = match &ident {
                        Some(ident) => Either::Left(ident),
                        None => Either::Right(Index::from(idx)),
                    };
                    let closure = attrs
                        .iter()
                        .find_map(|attr| {
                            attr.meta.path().is_ident("patch").then(
                                || match &attr.meta {
                                    Meta::List(list) => {
                                        match Punctuated::<
                                                ExprClosure,
                                                Comma,
                                            >::parse_terminated
                                                .parse2(list.tokens.clone())
                                            {
                                                Ok(closures) => {
                                                    let closure = closures.iter().next().cloned().expect_or_abort("should have ONE closure");
                                                    if closure.inputs.len() != 2 {
                                                        abort!(closure.inputs, "patch closure should have TWO params as in #[patch(|this, new| ...)]");
                                                    }
                                                    closure
                                                },
                                                Err(e) => abort!(list, e),
                                            }
                                    }
                                    _ => abort!(attr.meta, "needs to be as `#[patch(|this, new| ...)]`"),
                                },
                            )
                        });

                    if let Some(closure) = closure {
                        let params = closure.inputs;
                        let body = closure.body;
                        quote! {
                            if new.#locator != self.#locator {
                                _ = {
                                    let (#params) = (&mut self.#locator, new.#locator);
                                    #body
                                };
                                notify(&new_path);
                            }
                            new_path.replace_last(#idx + 1);
                        }
                    } else {
                        quote! {
                            #library_path::PatchField::patch_field(
                                &mut self.#locator,
                                new.#locator,
                                &new_path,
                                notify
                            );
                            new_path.replace_last(#idx + 1);
                        }
                    }
                }).collect::<Vec<_>>()
            }
            PatchModelTy::Enum { variants: _ } => {
                unreachable!("not implemented currently")
            }
        };

        // read access
        tokens.extend(quote! {
            #[automatically_derived]
            impl #library_path::PatchField for #name #generics
            {
                fn patch_field(
                    &mut self,
                    new: Self,
                    path: &#library_path::StorePath,
                    notify: &mut dyn FnMut(&#library_path::StorePath),
                ) {
                    let mut new_path = path.clone();
                    new_path.push(0);
                    #(#fields)*
                }
            }
        });
    }
}

enum Either<A, B> {
    Left(A),
    Right(B),
}

impl<A: ToTokens, B: ToTokens> ToTokens for Either<A, B> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Either::Left(a) => a.to_tokens(tokens),
            Either::Right(b) => b.to_tokens(tokens),
        }
    }
}
