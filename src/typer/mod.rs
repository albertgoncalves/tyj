#[cfg(test)]
mod test;

use crate::parser::{Expr, Stmt, Syntax};
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

const SHADOW_IDENT: &str = "Shadowed Identifier";
const UNKNOWN_IDENT: &str = "Unknown Identifier";
const DUPLICATE_KEYS: &str = "Duplicate Keys";

#[derive(Debug, PartialEq)]
pub(crate) struct Error<'a, 'b> {
    pub(crate) syntax: &'a Syntax<'a>,
    pub(crate) message: &'b str,
}

type TypeIndex = usize;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Obj(Rc<BTreeMap<&'a str, Type<'a>>>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Table<'a> {
    pub(crate) types: Vec<Type<'a>>,
    pub(crate) indices: HashMap<Vec<&'a str>, TypeIndex>,
}

fn set_index_from_expr<'a, 'b, 'c>(
    types: &'b mut Vec<Type<'a>>,
    indices: &'b mut HashMap<Vec<&'a str>, TypeIndex>,
    keys: &'b [&'a str],
    expr: &'a Expr,
) -> Result<(), &'c str> {
    let type_: Type = match expr {
        Expr::Ident(ident) => {
            let index: TypeIndex = match indices.get(&vec![*ident]) {
                Some(index) => *index,
                None => return Err(UNKNOWN_IDENT),
            };
            let _: Option<_> = indices.insert(keys.to_vec(), index);
            return Ok(());
        }
        Expr::Num(_) => Type::Num,
        Expr::Str(_) => Type::Str,
        Expr::Bool(_) => Type::Bool,
        Expr::Null => Type::Null,
        Expr::Undef => Type::Undef,
        Expr::Obj(props) => {
            let mut type_props: BTreeMap<&str, Type> = BTreeMap::new();
            for prop in props {
                let mut key: Vec<&str> = Vec::with_capacity(keys.len() + 1);
                key.extend_from_slice(keys);
                key.push(prop.key);
                set_index_from_expr(types, indices, &key, &prop.value)?;
                let value: Type = if let Some(value) = types.last() {
                    value.clone()
                } else {
                    unreachable!();
                };
                if let Some(_) = type_props.insert(prop.key, value) {
                    unreachable!();
                }
            }
            Type::Obj(Rc::new(type_props))
        }
        _ => panic!(format!("{:#?}", expr)),
    };
    let index: TypeIndex = types.len();
    types.push(type_);
    match indices.insert(keys.to_vec(), index) {
        Some(_) => Err(DUPLICATE_KEYS),
        None => Ok(()),
    }
}

pub(crate) fn get_types<'a, 'b>(
    ast: &'a [Syntax<'a>],
) -> Result<Table<'a>, Error<'a, 'b>> {
    let mut types: Vec<Type> = Vec::new();
    let mut indices: HashMap<Vec<&str>, TypeIndex> = HashMap::new();
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                let key: Vec<&str> = vec![ident];
                if indices.contains_key(&key) {
                    return Err(Error { syntax, message: SHADOW_IDENT });
                }
                if let Err(message) =
                    set_index_from_expr(&mut types, &mut indices, &key, expr)
                {
                    return Err(Error { syntax, message });
                }
            }
            _ => panic!(format!("{:#?}", syntax)),
        }
    }
    Ok(Table { types, indices })
}
