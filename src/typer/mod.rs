#[cfg(test)]
mod test;

use crate::parser::{Expr, Stmt, Syntax};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;

const SHADOW_IDENT: &str = "shadowed identifier";
const UNKNOWN_IDENT: &str = "unknown identifier";
const DUPLICATE_KEYS: &str = "duplicate keys";
const MULTI_TYPE_ARRAY: &str = "array contains multiple types";

#[derive(Debug, PartialEq)]
pub(crate) struct Error<'a, 'b> {
    pub(crate) syntax: &'a Syntax<'a>,
    pub(crate) message: &'b str,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Obj(Rc<BTreeMap<&'a str, Type<'a>>>),
    EmptyArray,
    Array(Rc<Type<'a>>),
}

fn get_expr<'a, 'b, 'c>(
    types: &'b HashMap<Vec<&'a str>, Type<'a>>,
    expr: &'a Expr<'a>,
) -> Result<Type<'a>, &'c str> {
    Ok(match expr {
        Expr::Ident(ident) => match types.get(&vec![*ident]) {
            Some(type_) => type_.clone(),
            None => return Err(UNKNOWN_IDENT),
        },
        Expr::Num(_) => Type::Num,
        Expr::Str(_) => Type::Str,
        Expr::Bool(_) => Type::Bool,
        Expr::Null => Type::Null,
        Expr::Undef => Type::Undef,
        Expr::Obj(parse_props) => {
            let mut type_props: BTreeMap<&str, Type> = BTreeMap::new();
            for prop in parse_props {
                if let Some(_) =
                    type_props.insert(prop.key, get_expr(types, &prop.value)?)
                {
                    return Err(DUPLICATE_KEYS);
                }
            }
            Type::Obj(Rc::new(type_props))
        }
        Expr::Array(elems) => {
            let mut type_elems: BTreeSet<Type> = BTreeSet::new();
            for elem in elems {
                let _: bool = type_elems.insert(get_expr(types, elem)?);
            }
            let mut type_: Type = Type::EmptyArray;
            for elem in type_elems.iter() {
                match type_ {
                    Type::EmptyArray => {
                        type_ = Type::Array(Rc::new(elem.clone()))
                    }
                    _ => return Err(MULTI_TYPE_ARRAY),
                }
            }
            type_
        }
        _ => panic!("{:#?}", expr),
    })
}

fn set_type<'a, 'b, 'c>(
    types: &'b mut HashMap<Vec<&'a str>, Type<'a>>,
    keys: &'b [&'a str],
    type_: &'b Type<'a>,
) -> Result<(), &'c str> {
    if let Type::Obj(props) = type_ {
        for (prop_key, prop_value) in props.iter() {
            let mut key: Vec<&str> = Vec::with_capacity(keys.len() + 1);
            key.extend_from_slice(keys);
            key.push(prop_key);
            set_type(types, &key, &prop_value)?;
        }
    }
    match types.insert(keys.to_vec(), type_.clone()) {
        Some(_) => Err(DUPLICATE_KEYS),
        None => Ok(()),
    }
}

pub(crate) fn get_types<'a, 'b>(
    ast: &'a [Syntax<'a>],
) -> Result<HashMap<Vec<&'a str>, Type<'a>>, Error<'a, 'b>> {
    let mut types: HashMap<Vec<&str>, Type> = HashMap::new();
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                let key: Vec<&str> = vec![ident];
                if types.contains_key(&key) {
                    return Err(Error { syntax, message: SHADOW_IDENT });
                }
                match get_expr(&types, expr) {
                    Err(message) => return Err(Error { syntax, message }),
                    Ok(type_) => {
                        if let Err(message) =
                            set_type(&mut types, &key, &type_)
                        {
                            return Err(Error { syntax, message });
                        }
                    }
                }
            }
            _ => panic!("{:#?}", syntax),
        }
    }
    Ok(types)
}
