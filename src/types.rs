use std::collections::BTreeMap;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Target<'a> {
    pub(crate) ident: Vec<&'a str>,
    pub(crate) scope: Vec<&'a str>,
}

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

#[macro_export]
macro_rules! btree_map {
    ($($tuple:expr),+ $(,)?) => (
        vec![$($tuple),+].into_iter().collect()
    );
}
