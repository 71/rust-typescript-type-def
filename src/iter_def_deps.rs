use crate::type_expr::{
    DefinedTypeInfo, IndexSignature, NativeTypeInfo, ObjectField, TypeArray,
    TypeDefinition, TypeExpr, TypeInfo, TypeIntersection, TypeName, TypeObject,
    TypeString, TypeTuple, TypeUnion,
};
use std::{
    collections::{HashSet, VecDeque},
    iter::{self, FusedIterator},
    slice,
};

#[derive(Clone, Copy, Debug)]
enum TypeExprOrTypeInfoRef {
    TypeExpr(TypeExpr),
    TypeInfoRef(&'static TypeInfo),
}

impl TypeExprOrTypeInfoRef {
    fn as_ref(&self) -> Result<&'static TypeInfo, &TypeExpr> {
        match self {
            TypeExprOrTypeInfoRef::TypeInfoRef(r) => Ok(r),
            TypeExprOrTypeInfoRef::TypeExpr(TypeExpr::Ref(r)) => Ok(r()),
            TypeExprOrTypeInfoRef::TypeExpr(expr) => Err(expr),
        }
    }
}

/// An iterator which produces all type definitions that a type depends on.
///
/// Type definitions dependencies (including those of generic types) are
/// produced exactly once.
pub struct IterDefDeps {
    stack: VecDeque<TypeExprOrTypeInfoRef>,
    emitted: HashSet<*const TypeDefinition>,
}

impl IterDefDeps {
    /// Creates a new iterator of the dependencies of the given type info.
    pub fn new(roots: &[&'static TypeInfo]) -> Self {
        Self {
            stack: roots
                .iter()
                .map(|x| TypeExprOrTypeInfoRef::TypeInfoRef(x))
                .collect(),
            emitted: HashSet::new(),
        }
    }
}

impl Iterator for IterDefDeps {
    type Item = &'static TypeDefinition;

    fn next(&mut self) -> Option<Self::Item> {
        let Self { stack, emitted } = self;

        while let Some(expr) = stack.pop_front() {
            stack.extend(
                TypeExprChildren::new(&expr)
                    .map(|x| TypeExprOrTypeInfoRef::TypeExpr(*x)),
            );

            if let Ok(TypeInfo::Defined(DefinedTypeInfo {
                def:
                    def @ TypeDefinition {
                        docs: _,
                        path: _,
                        name: _,
                        generic_vars: _,
                        def: def_expr,
                    },
                generic_args: _,
            })) = expr.as_ref()
            {
                if !emitted.insert(*def) {
                    continue;
                }

                stack.push_back(TypeExprOrTypeInfoRef::TypeExpr(*def_expr));

                return Some(def);
            }
        }

        None
    }
}

impl FusedIterator for IterDefDeps {}

/// An iterator which produces all of the direct type expression children of a
/// type expression.
enum TypeExprChildren<'a> {
    None,
    One(iter::Once<&'a TypeExpr>),
    Slice(slice::Iter<'a, TypeExpr>),
    Object(Option<&'a IndexSignature>, slice::Iter<'a, ObjectField>),
}

impl<'a> TypeExprChildren<'a> {
    fn new(expr: &'a TypeExprOrTypeInfoRef) -> Self {
        match expr.as_ref() {
            Ok(TypeInfo::Native(NativeTypeInfo { r#ref })) => {
                Self::One(iter::once(r#ref))
            }
            Ok(TypeInfo::Defined(DefinedTypeInfo {
                def: _,
                generic_args,
            })) => Self::Slice(generic_args.iter()),
            Err(TypeExpr::Ref(_)) => unreachable!("handled above"),
            Err(TypeExpr::Name(TypeName {
                path: _,
                name: _,
                generic_args,
            })) => Self::Slice(generic_args.iter()),
            Err(TypeExpr::String(TypeString { docs: _, value: _ })) => {
                Self::None
            }
            Err(TypeExpr::Tuple(TypeTuple { docs: _, elements })) => {
                Self::Slice(elements.iter())
            }
            Err(TypeExpr::Object(TypeObject {
                docs: _,
                index_signature,
                fields,
            })) => Self::Object(index_signature.as_ref(), fields.iter()),
            Err(TypeExpr::Array(TypeArray { docs: _, item })) => {
                Self::One(iter::once(item))
            }
            Err(TypeExpr::Union(TypeUnion { docs: _, members })) => {
                Self::Slice(members.iter())
            }
            Err(TypeExpr::Intersection(TypeIntersection {
                docs: _,
                members,
            })) => Self::Slice(members.iter()),
        }
    }
}

impl<'a> Iterator for TypeExprChildren<'a> {
    type Item = &'a TypeExpr;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::None => None,
            Self::One(iter) => iter.next(),
            Self::Slice(iter) => iter.next(),
            Self::Object(index_signature, iter) => index_signature
                .take()
                .map(
                    |IndexSignature {
                         docs: _,
                         name: _,
                         value,
                     }| *value,
                )
                .or_else(|| {
                    iter.next().map(
                        |ObjectField {
                             docs: _,
                             name: _,
                             optional: _,
                             r#type,
                         }| r#type,
                    )
                }),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::None => (0, Some(0)),
            Self::One(iter) => iter.size_hint(),
            Self::Slice(iter) => iter.size_hint(),
            Self::Object(index_signature, iter) => {
                let (min, max) = iter.size_hint();
                if index_signature.is_some() {
                    (
                        min.saturating_add(1),
                        max.and_then(|max| max.checked_add(1)),
                    )
                } else {
                    (min, max)
                }
            }
        }
    }
}

impl FusedIterator for TypeExprChildren<'_> {}

impl ExactSizeIterator for TypeExprChildren<'_> {}
