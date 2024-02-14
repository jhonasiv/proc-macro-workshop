use quote::ToTokens;
use syn::spanned::Spanned;

// struct AssociativeType {
//     trait_ident: syn::Ident,
//     colon2: syn::token::PathSep,
//     item_ident: syn::Ident,
// }

// impl syn::parse::Parse for AssociativeType {
//     fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
//         let trait_ident = input.parse()?;
//         let colon2 = input.parse()?;
//         let item_ident = input.parse()?;
//         Ok(Self {
//             trait_ident,
//             colon2,
//             item_ident,
//         })
//     }
// }

// // impl std::convert::Into<syn::Type> for AssociativeType {
// //     fn into(self) -> syn::Type {
// //         syn::Type::Path(syn::TypePath{
// //             qself: None,
// //             path: syn::Path::
// //         })
// //     }
// // }

// impl quote::ToTokens for AssociativeType {
//     fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
//         self.trait_ident.to_tokens(tokens);
//         self.colon2.to_tokens(tokens);
//         self.item_ident.to_tokens(tokens);
//     }
// }

// struct BoundAssociativeType {
//     associative_ty: AssociativeType,
//     colon: syn::Token![:],
//     binding: syn::Path,
// }

// impl syn::parse::Parse for BoundAssociativeType {
//     fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
//         let associative_ty = input.parse()?;
//         let colon = input.parse()?;
//         let binding = input.parse()?;
//         Ok(Self {
//             associative_ty,
//             colon,
//             binding,
//         })
//     }
// }

struct ImplData<'a> {
    generics: &'a syn::Generics,
    fields: &'a ::std::vec::Vec<FieldsData>,
    fmt_str: ::std::vec::Vec<::std::option::Option<::std::string::String>>,
}

impl<'a> ImplData<'a> {
    pub fn new(
        generics: &'a syn::Generics,
        fields: &'a ::std::vec::Vec<FieldsData>,
    ) -> Self {
        let fmt_str = Self::generate_fmt_string(fields);
        Self {
            generics,
            fields,
            fmt_str,
        }
    }

    fn generate_fmt_string(
        fields: &::std::vec::Vec<FieldsData>,
    ) -> ::std::vec::Vec<::std::option::Option<::std::string::String>> {
        fields
            .iter()
            .map(|f| {
                let out = format!(
                    "{}",
                    f.fmt_pattern.as_ref().map(|f| f.as_str()).unwrap_or("{:?}")
                );
                Some(out)
            })
            .collect()
    }

    pub fn generate(&self, struct_name: &syn::Ident) -> proc_macro2::TokenStream {
        let (debug_fields, debug_fmt_str): (
            Vec<&FieldsData>,
            Vec<&::std::string::String>,
        ) = self
            .fields
            .iter()
            .zip(&self.fmt_str)
            .filter(|(f, fmt_str)| {
                if let syn::Type::Path(syn::TypePath { path, .. }) = &f.ty {
                    return fmt_str.is_some()
                        && !path.segments.iter().any(|s| s.ident == "PhantomData");
                }
                fmt_str.is_some()
            })
            .map(|(f, fmt_str)| (f, fmt_str.as_ref().unwrap()))
            .unzip();

        let field_names: ::std::vec::Vec<&::std::option::Option<proc_macro2::Ident>> =
            debug_fields.iter().map(|f| &f.ident).collect();

        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        quote::quote!(
            impl #impl_generics ::std::fmt::Debug for #struct_name #ty_generics #where_clause {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    f.debug_struct(stringify!(#struct_name))
                     #(.field(stringify!(#field_names), &format_args!(#debug_fmt_str, self.#field_names)))*
                     .finish()
                }
            }

        )
    }
}

struct FieldsData {
    ident: ::std::option::Option<syn::Ident>,
    fmt_pattern: ::std::option::Option<::std::string::String>,
    ty: ::syn::Type,
}

enum GenericVariant {
    Associative(syn::Type),
    SameGeneric,
    Phantom,
    DifferentFromGeneric,
}

impl GenericVariant {
    pub fn prioritize(new_variant: Self, old_variant: Self) -> Self {
        match (&new_variant, &old_variant) {
            (_, Self::Phantom) => Self::Phantom,
            (Self::Phantom, _) => Self::Phantom,
            (_, Self::Associative(_)) => old_variant,
            (Self::Associative(_), _) => new_variant,
            (_, Self::SameGeneric) => Self::SameGeneric,
            (Self::SameGeneric, _) => Self::SameGeneric,
            _ => new_variant,
        }
    }

    // pub fn display(&self) {
    //     match &self {
    //         Self::Phantom => println!("PHANTOM"),
    //         Self::Associative(a) => println!("ASSOCIATIVE {}", a.to_token_stream()),
    //         Self::SameGeneric => println!("SAME GENERIC"),
    //         Self::DifferentFromGeneric => println!("DIFFERENT FROM THE GENERIC"),
    //     }
    // }
}

#[proc_macro_derive(CustomDebug, attributes(debug, bound))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let struct_name = ast.ident;
    let fields_info = match &ast.data {
        ::syn::Data::Struct(data_struct) => match &data_struct.fields {
            ::syn::Fields::Named(field) => field.named.clone(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };
    // process_outer_attrs(ast.attrs);
    let (fields, _possible_errors): (
        ::std::vec::Vec<FieldsData>,
        ::std::vec::Vec<::std::option::Option<::syn::Error>>,
    ) = process_fields(&fields_info).into_iter().unzip();
    let generics = ast.generics;
    let field_types = fields.iter().map(|f| f.ty.clone()).collect();
    let generics_or_err = compute_trait_bounds(generics, &field_types);
    if let Err(err) = generics_or_err {
        let quote_err = err.to_compile_error();
        return proc_macro::TokenStream::from(quote::quote!(#quote_err));
    }
    let generics = generics_or_err.unwrap();

    let impl_data = generate_impl_data(&generics, &fields);
    let impls = impl_data.generate(&struct_name);
    let expanded = quote::quote!(
        #impls
    );
    proc_macro::TokenStream::from(expanded)
}

// fn process_outer_attrs(
//     attrs: ::std::vec::Vec<syn::Attribute>,
// ) -> ::std::result::Result<(), syn::Error> {
//     for attr in attrs {
//         println!("{}", attr.to_token_stream());
//         if attr.path().is_ident("debug") {
//             let mut bound_ty = String::new();
//             let mut binding = String::new();
//             attr.parse_nested_meta(|meta| {
//                 if meta.path.is_ident("bound") {
//                     let value = meta.value()?;
//                     println!("TYPE VALUE {}", value);
//                     Ok(())
//                 } else {
//                     Err(syn::Error::new(meta.path.span(), "unsupported attribute"))
//                 }
//             })?
//         }
//     }
//     unimplemented!()
// }

fn process_fields(
    ast_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> ::std::vec::Vec<(FieldsData, ::std::option::Option<::syn::Error>)> {
    ast_fields
        .iter()
        .map(|field| {
            let ident = field.ident.clone();
            let mut error: ::std::option::Option<::syn::Error> = None;
            let fmt_pattern = match debug_attrs(&field.attrs) {
                Ok(p) => p,
                Err(e) => {
                    error = Some(e);
                    None
                },
            };
            (
                FieldsData {
                    ident,
                    fmt_pattern,
                    ty: field.ty.clone(),
                },
                error,
            )
        })
        .collect()
}

fn debug_attrs(
    attributes: &Vec<syn::Attribute>,
) -> ::std::result::Result<Option<String>, syn::Error> {
    for attr in attributes {
        if attr.path().is_ident("debug") {
            let literal_string = match &attr.meta {
                syn::Meta::NameValue(syn::MetaNameValue {
                    value:
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(lit_str),
                            ..
                        }),
                    ..
                }) => Ok(lit_str.value()),
                _ => Err(syn::Error::new(attr.span(), "invalid attribute syntax")),
            }?;

            return Ok(Some(literal_string));
        }
    }
    Ok(None)
}

fn compute_trait_bounds(
    mut generics: syn::Generics,
    types: &::std::vec::Vec<syn::Type>,
) -> ::std::result::Result<syn::Generics, syn::Error> {
    let mut where_generics = generics.clone();
    for param in &mut generics.params {
        match param {
            syn::GenericParam::Type(ref mut type_param) => {
                let generic_ident = &type_param.ident;
                let generic_variant = compute_generic_variant(&types, generic_ident);
                if generic_variant.iter().any(|v| match v {
                    GenericVariant::SameGeneric => true,
                    _ => false,
                }) {
                    type_param.bounds.push(syn::parse_quote!(::std::fmt::Debug));
                }
                if let Some(GenericVariant::Associative(parsed_ty)) =
                    generic_variant.iter().find(|v| match v {
                        GenericVariant::Associative(_) => true,
                        _ => false,
                    })
                {
                    let where_clause = where_generics.make_where_clause();
                    if !where_clause.predicates.iter().any(|pred| {
                        if let syn::WherePredicate::Type(syn::PredicateType {
                            bounded_ty,
                            ..
                        }) = pred
                        {
                            quote::quote!(#bounded_ty).to_string()
                                == quote::quote!(#parsed_ty).to_string()
                        } else {
                            false
                        }
                    }) {
                        let bound = syn::TypeParamBound::Trait(syn::TraitBound {
                            paren_token: None,
                            modifier: syn::TraitBoundModifier::None,
                            lifetimes: None,
                            path: syn::parse_quote!(::std::fmt::Debug),
                        });
                        let mut bounds: syn::punctuated::Punctuated<
                            syn::TypeParamBound,
                            syn::Token![+],
                        > = syn::punctuated::Punctuated::new();
                        bounds.push(bound);
                        where_clause.predicates.push(syn::WherePredicate::Type(
                            syn::PredicateType {
                                lifetimes: None,
                                bounded_ty: parsed_ty.clone(),
                                colon_token: syn::token::Colon(parsed_ty.span()),
                                bounds,
                            },
                        ))
                    } else {
                        where_clause.predicates.iter_mut().for_each(|pred| {
                            if let syn::WherePredicate::Type(syn::PredicateType {
                                ref bounded_ty,

                                bounds,
                                ..
                            }) = pred
                            {
                                if quote::quote!(#bounded_ty).to_string()
                                    == quote::quote!(#parsed_ty).to_string()
                                {
                                    bounds.push(syn::parse_quote!(::std::fmt::Debug));
                                }
                            }
                        })
                    };
                }
            },
            _ => (),
        };
    }
    generics.where_clause = where_generics.where_clause;
    Ok(generics)
}

fn compute_generic_variant(
    types: &::std::vec::Vec<syn::Type>,
    generic_ident: &syn::Ident,
) -> ::std::vec::Vec<GenericVariant> {
    types
        .iter()
        .map(|ty| match ty {
            syn::Type::Path(syn::TypePath {
                path: syn::Path { segments, .. },
                ..
            }) => segments.iter().fold(
                GenericVariant::DifferentFromGeneric,
                |variant, segment| {
                    // Associative type from this generic
                    let inner_variant = if let Ok(associative_type) =
                        syn::parse2(segments.to_token_stream())
                    {
                        GenericVariant::Associative(associative_type)
                    } else if &segment.ident == generic_ident {
                        GenericVariant::SameGeneric
                    } else {
                        recursive_search_over_arguments_for_type(
                            &segment.arguments,
                            &generic_ident,
                        )
                    };
                    GenericVariant::prioritize(inner_variant, variant)
                },
            ),
            _ => GenericVariant::DifferentFromGeneric,
        })
        .collect()
}

fn recursive_search_over_arguments_for_type(
    args: &syn::PathArguments,
    generic_ident: &syn::Ident,
) -> GenericVariant {
    if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        args: inner_args,
        ..
    }) = args
    {
        inner_args.iter().fold(
            GenericVariant::DifferentFromGeneric,
            |arg_variant, arg| match arg {
                syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                    path: syn::Path { segments, .. },
                    ..
                })) => {
                    // Associative type from this generic
                    let seg_variant = if let Ok(associative_type) =
                        syn::parse2(segments.to_token_stream())
                    {
                        GenericVariant::Associative(associative_type)
                    } else {
                        segments.iter().fold(
                            GenericVariant::DifferentFromGeneric,
                            |variant, s| {
                                let inner_variant = if &s.ident == generic_ident {
                                    GenericVariant::SameGeneric
                                } else {
                                    recursive_search_over_arguments_for_type(
                                        &s.arguments,
                                        generic_ident,
                                    )
                                };

                                GenericVariant::prioritize(inner_variant, variant)
                            },
                        )
                    };
                    GenericVariant::prioritize(seg_variant, arg_variant)
                },
                _ => arg_variant,
            },
        )
    } else {
        GenericVariant::DifferentFromGeneric
    }
}

fn generate_impl_data<'a>(
    generics: &'a ::syn::Generics,
    fields: &'a ::std::vec::Vec<FieldsData>,
) -> ImplData<'a> {
    ImplData::new(generics, fields)
}
