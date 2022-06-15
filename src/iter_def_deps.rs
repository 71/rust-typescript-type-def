use crate::type_expr::{
    DefinedTypeInfo, Ident, IndexSignature, NativeTypeInfo, ObjectField,
    TypeArray, TypeDefinition, TypeExpr, TypeInfo, TypeIntersection, TypeName,
    TypeObject, TypeString, TypeTuple, TypeUnion,
};
use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
    iter::{self, FusedIterator},
    slice,
};

#[derive(Clone, Copy, Debug)]
enum TypeExprOrTypeInfoRef {
    TypeExpr(TypeExpr),
    TypeInfoRef(&'static TypeInfo),
}

#[derive(Clone, Copy)]
enum TypeExprRefOrTypeInfoRef<'a> {
    TypeExprRef(&'a TypeExpr),
    TypeInfoRef(&'static TypeInfo),
}

impl<'a> From<&'a TypeExprOrTypeInfoRef> for TypeExprRefOrTypeInfoRef<'a> {
    fn from(v: &'a TypeExprOrTypeInfoRef) -> Self {
        match v {
            TypeExprOrTypeInfoRef::TypeExpr(e) => TypeExprRefOrTypeInfoRef::TypeExprRef(e),
            TypeExprOrTypeInfoRef::TypeInfoRef(r) => TypeExprRefOrTypeInfoRef::TypeInfoRef(r),
        }
    }
}

impl<'a> From<&'a TypeExpr> for TypeExprRefOrTypeInfoRef<'a> {
    fn from(v: &'a TypeExpr) -> Self {
        TypeExprRefOrTypeInfoRef::TypeExprRef(v)
    }
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

impl<'a> TypeExprRefOrTypeInfoRef<'a> {
    fn as_ref(&self) -> Result<&'static TypeInfo, &'a TypeExpr> {
        match self {
            TypeExprRefOrTypeInfoRef::TypeInfoRef(r) => Ok(r),
            TypeExprRefOrTypeInfoRef::TypeExprRef(TypeExpr::Ref(r)) => Ok(r()),
            TypeExprRefOrTypeInfoRef::TypeExprRef(expr) => Err(expr),
        }
    }
}

/// An iterator which produces all type definitions that a type depends on.
///
/// Type definitions dependencies (including those of generic types) are
/// produced exactly once.
pub struct IterDefDeps {
    stack: VecDeque<TypeExprOrTypeInfoRef>,
    visited: HashSet<u64>,
    emitted: HashSet<*const TypeDefinition>,
}

impl IterDefDeps {
    /// Creates a new iterator of the dependencies of the given type info.
    pub fn new(roots: &[&'static TypeInfo]) -> Self {
        Self {
            stack: roots.iter().map(|x| TypeExprOrTypeInfoRef::TypeInfoRef(x)).collect(),
            visited: HashSet::new(),
            emitted: HashSet::new(),
        }
    }
}

impl Iterator for IterDefDeps {
    type Item = &'static TypeDefinition;

    fn next(&mut self) -> Option<Self::Item> {
        let Self {
            stack,
            visited,
            emitted,
        } = self;

        while let Some(expr) = stack.pop_front() {
            let expr_hash = hash_type_expr((&expr).into());

            if !visited.insert(expr_hash) {
                continue;
            }

            stack.extend(
                TypeExprChildren::new(&expr)
                    .filter(|expr| {
                        !visited.contains(&hash_type_expr(
                            (*expr).into(),
                        ))
                    })
                    .map(|x| TypeExprOrTypeInfoRef::TypeExpr(*x)),
            );

            if let Ok(TypeInfo::Defined(DefinedTypeInfo {
                def,
                generic_args: _,
            })) = expr.as_ref()
            {
                if emitted.insert(*def) {
                    return Some(def);
                }
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
    OneThenSlice(
        iter::Chain<iter::Once<&'a TypeExpr>, slice::Iter<'a, TypeExpr>>,
    ),
    Object(Option<&'a IndexSignature>, slice::Iter<'a, ObjectField>),
}

impl<'a> TypeExprChildren<'a> {
    fn new(expr: &'a TypeExprOrTypeInfoRef) -> Self {
        match expr.as_ref() {
            Ok(TypeInfo::Native(NativeTypeInfo { r#ref })) => {
                Self::One(iter::once(r#ref))
            }
            Ok(TypeInfo::Defined(DefinedTypeInfo {
                def:
                    TypeDefinition {
                        docs: _,
                        path: _,
                        name: _,
                        generic_vars: _,
                        def,
                    },
                generic_args,
            })) => {
                Self::OneThenSlice(iter::once(def).chain(generic_args.iter()))
            }
            Err(TypeExpr::Ref(_)) => unreachable!("handled above"),
            Err(TypeExpr::Name(TypeName {
                path: _,
                name: _,
                generic_args,
            })) => Self::Slice(generic_args.iter()),
            Err(TypeExpr::String(TypeString { docs: _, value: _ })) => Self::None,
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
            Err(TypeExpr::Intersection(TypeIntersection { docs: _, members })) => {
                Self::Slice(members.iter())
            }
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
            Self::OneThenSlice(iter) => iter.next(),
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
            Self::OneThenSlice(iter) => iter.size_hint(),
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

impl DoubleEndedIterator for TypeExprChildren<'_> {
    fn next_back(&mut self) -> Option<<Self as Iterator>::Item> {
        match self {
            Self::None => None,
            Self::One(iter) => iter.next_back(),
            Self::Slice(iter) => iter.next_back(),
            Self::OneThenSlice(iter) => iter.next_back(),
            Self::Object(index_signature, iter) => iter
                .next_back()
                .map(
                    |ObjectField {
                         docs: _,
                         name: _,
                         optional: _,
                         r#type,
                     }| { r#type },
                )
                .or_else(|| {
                    index_signature.take().map(
                        |IndexSignature {
                             docs: _,
                             name: _,
                             value,
                         }| *value,
                    )
                }),
        }
    }
}

fn hash_type_expr(expr: TypeExprRefOrTypeInfoRef<'_>) -> u64 {
    use std::{collections::hash_map::DefaultHasher, hash::Hasher};

    fn visit_expr<'a>(
        expr: impl Into<TypeExprRefOrTypeInfoRef<'a>>,
        state: &mut DefaultHasher,
    ) {
        match expr.into().as_ref() {
            Ok(type_info) => {
                (type_info as *const TypeInfo).hash(state);
            }
            Err(TypeExpr::Ref(_)) => unreachable!("handled above"),
            Err(TypeExpr::Name(TypeName {
                path,
                name: Ident(name),
                generic_args,
            })) => {
                1.hash(state);
                for Ident(path_part) in *path {
                    path_part.hash(state);
                }
                name.hash(state);
                for generic_arg in *generic_args {
                    visit_expr(generic_arg, state);
                }
            }
            Err(TypeExpr::String(TypeString { docs: _, value })) => {
                2.hash(state);
                value.hash(state);
            }
            Err(TypeExpr::Tuple(TypeTuple { docs: _, elements })) => {
                3.hash(state);
                for element in *elements {
                    visit_expr(element, state);
                }
            }
            Err(TypeExpr::Object(TypeObject {
                docs: _,
                index_signature,
                fields,
            })) => {
                4.hash(state);
                if let Some(IndexSignature {
                    docs: _,
                    name: Ident(name),
                    value,
                }) = index_signature
                {
                    name.hash(state);
                    visit_expr(*value, state);
                }
                for ObjectField {
                    docs: _,
                    name:
                        TypeString {
                            docs: _,
                            value: name,
                        },
                    optional,
                    r#type,
                } in *fields
                {
                    name.hash(state);
                    optional.hash(state);
                    visit_expr(r#type, state);
                }
            }
            Err(TypeExpr::Array(TypeArray { docs: _, item })) => {
                5.hash(state);
                visit_expr(*item, state);
            }
            Err(TypeExpr::Union(TypeUnion { docs: _, members })) => {
                6.hash(state);
                for member in *members {
                    visit_expr(member, state);
                }
            }
            Err(TypeExpr::Intersection(TypeIntersection { docs: _, members })) => {
                7.hash(state);
                for member in *members {
                    visit_expr(member, state);
                }
            }
        }
    }

    let mut hasher = DefaultHasher::new();
    visit_expr(expr, &mut hasher);
    hasher.finish()
}
