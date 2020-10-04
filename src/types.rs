use std::collections::BTreeMap;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Type<'a> {
    Array(Box<Type<'a>>),
    Bool,
    EmptyArray,
    Fn(BTreeMap<Vec<Type<'a>>, Type<'a>>),
    Null,
    Num,
    Obj(BTreeMap<&'a str, Type<'a>>),
    Str,
    Undef,
    Uninit,
}
