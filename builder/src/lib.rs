use ::syn::spanned::Spanned;

enum Repeatable {
    No,
    SingularOnly(::proc_macro2::Ident, ::syn::Type),
    SingularAndPlural(::proc_macro2::Ident, ::syn::Type),
}

struct FieldData {
    ident: Option<::syn::Ident>,
    ty: ::syn::Type,
    repeat: Repeatable,
    optional: bool,
}

impl FieldData {
    pub fn generate_builder_field(&self) -> ::proc_macro2::TokenStream {
        let initial_value = if let Repeatable::No = &self.repeat {
            ::quote::quote!(::std::option::Option::None)
        } else {
            ::quote::quote!(::std::option::Option::Some(vec![]))
        };
        let ident = &self.ident;
        ::quote::quote!(#ident: #initial_value)
    }

    pub fn generate_setter(&self) -> ::proc_macro2::TokenStream {
        let ident = &self.ident;
        let ty = &self.ty;
        match &self.repeat {
            Repeatable::No => {
                ::quote::quote!(
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self{
                        self.#ident = ::std::option::Option::Some(#ident);
                        self
                    }
                )
            },
            Repeatable::SingularOnly(s, new_ty) => {
                ::quote::quote!(
                    pub fn #s(&mut self, #s: #new_ty) -> &mut Self {
                        self.#ident.get_or_insert_with(::std::vec::Vec::new).push(#s);
                        self
                    }
                )
            },
            Repeatable::SingularAndPlural(s, new_ty) => {
                ::quote::quote!(
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = ::std::option::Option::Some(#ident);
                        self
                    }

                    pub fn #s(&mut self, #s: #new_ty) -> &mut Self {
                        self.#ident.get_or_insert_with(::std::vec::Vec::new).push(#s);
                        self
                    }
                )
            },
        }
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> ::proc_macro::TokenStream {
    let input = ::syn::parse_macro_input!(input as ::syn::DeriveInput);
    let name = &input.ident;
    let builder_name = ::quote::format_ident!("{}Builder", name);
    let fields_info = match &input.data {
        ::syn::Data::Struct(data_struct) => match &data_struct.fields {
            ::syn::Fields::Named(field) => field.named.clone(),
            ::syn::Fields::Unnamed(field) => field.unnamed.clone(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let (fields_data, possible_errors): (
        ::std::vec::Vec<FieldData>,
        ::std::vec::Vec<::std::option::Option<::syn::Error>>,
    ) = process_fields(fields_info).into_iter().unzip();
    let errors: ::std::vec::Vec<::proc_macro2::TokenStream> = possible_errors
        .iter()
        .flatten()
        .map(|e| e.to_compile_error())
        .collect();
    let fields_names: ::std::vec::Vec<::std::option::Option<::proc_macro2::Ident>> =
        fields_data.iter().map(|f| f.ident.clone()).collect();
    let fields_types = fields_data.iter().map(|f| f.ty.clone());
    let optional_fields: ::std::vec::Vec<::std::option::Option<::proc_macro2::Ident>> =
        fields_data
            .iter()
            .filter(|f| f.optional)
            .map(|f| f.ident.clone())
            .collect();
    let required_fields: ::std::vec::Vec<::std::option::Option<::proc_macro2::Ident>> =
        fields_data
            .iter()
            .filter(|f| !f.optional)
            .map(|f| f.ident.clone())
            .collect();
    let setters: ::std::vec::Vec<::proc_macro2::TokenStream> =
        fields_data.iter().map(|f| f.generate_setter()).collect();
    let builder_fields: ::std::vec::Vec<::proc_macro2::TokenStream> = fields_data
        .iter()
        .map(|f| f.generate_builder_field())
        .collect();

    let expanded = ::quote::quote! {
        #(#errors)*
        pub struct #builder_name {
            #(#fields_names: ::std::option::Option<#fields_types>),*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_fields),*
                }
            }
        }

        impl #builder_name {
            #(#setters

            )*

            pub fn build(&self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(let #required_fields = self.#required_fields.to_owned().ok_or(format!("{} must be set before building", stringify!(#required_fields)))?;)*
                #(let #optional_fields = self.#optional_fields.to_owned();)*
                ::std::result::Result::Ok(#name {
                    #(#required_fields: #required_fields.clone(),)*
                    #(#optional_fields: #optional_fields.clone()),*
                })
            }
        }

    };

    ::proc_macro::TokenStream::from(expanded)
}

fn process_fields(
    fields: ::syn::punctuated::Punctuated<::syn::Field, ::syn::token::Comma>,
) -> ::std::vec::Vec<(FieldData, ::std::option::Option<::syn::Error>)> {
    fields
        .iter()
        .map(|field| {
            let ident = field.ident.clone();
            let optional_ty = optional_type(&field.ty);
            let ty = if let Some(t) = optional_ty.clone() {
                t
            } else {
                field.ty.clone()
            };
            let mut error: ::std::option::Option<::syn::Error> = None;
            let repeat = match attr_each(ident.clone(), &ty, field.attrs.clone()) {
                Ok(r) => r,
                Err(e) => {
                    error = Some(e);
                    Repeatable::No
                },
            };

            let optional = optional_ty.is_some();
            (
                FieldData {
                    ident: ident.clone(),
                    ty,
                    repeat,
                    optional,
                },
                error,
            )
        })
        .collect()
}

fn optional_type(ty: &::syn::Type) -> ::std::option::Option<::syn::Type> {
    let segments = match ty {
        ::syn::Type::Path(::syn::TypePath {
            path: ::syn::Path { segments, .. },
            ..
        }) if segments.len() == 1 => segments.clone(),
        _ => return None,
    };

    let args = match &segments[0] {
        ::syn::PathSegment {
            ident,
            arguments:
                ::syn::PathArguments::AngleBracketed(::syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }),
        } if ident == "Option" && args.len() == 1 => args,
        _ => return None,
    };

    match &args[0] {
        ::syn::GenericArgument::Type(t) => Some(t.clone()),
        _ => None,
    }
}

fn attr_each(
    ident: ::std::option::Option<::proc_macro2::Ident>,
    ty: &::syn::Type,
    attrs: ::std::vec::Vec<::syn::Attribute>,
) -> Result<Repeatable, ::syn::Error> {
    // TODO: expand with more attributes
    for attr in attrs {
        if attr.path().is_ident("builder") {
            let mut literal_str: String = String::from("");
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("each") {
                    let value = meta.value()?;
                    let s: ::syn::LitStr = value.parse()?;
                    literal_str = s.value();
                    Ok(())
                } else {
                    Err(::syn::Error::new(
                        meta.path.span(),
                        "expected builder attribute `each = \"...\"`",
                    ))
                }
            })?;

            let ::syn::Type::Path {
                0:
                    ::syn::TypePath {
                        path: ::syn::Path { segments, .. },
                        ..
                    },
                ..
            } = ty
            else {
                return Ok(Repeatable::No);
            };
            let ::syn::PathSegment {
                ident: arg_ident,
                arguments:
                    ::syn::PathArguments::AngleBracketed(
                        ::syn::AngleBracketedGenericArguments { args, .. },
                    ),
            } = &segments[0]
            else {
                return Ok(Repeatable::No);
            };
            if arg_ident != "Vec" {
                return Ok(Repeatable::No);
            }
            let ::syn::GenericArgument::Type(inner_ty) = &args[0] else {
                return Ok(Repeatable::No);
            };
            if ident.as_ref().is_some_and(|i| literal_str == i.to_string()) {
                return Ok(Repeatable::SingularOnly(
                    ::syn::Ident::new(&*literal_str, literal_str.span()),
                    inner_ty.clone(),
                ));
            }
            return Ok(Repeatable::SingularAndPlural(
                ::syn::Ident::new(&*literal_str, literal_str.span()),
                inner_ty.clone(),
            ));
        }
    }

    return Ok(Repeatable::No);
}
