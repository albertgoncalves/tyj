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

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Obj(Rc<BTreeMap<&'a str, Type<'a>>>),
}

fn set_expr<'a, 'b, 'c>(
    types: &'b mut HashMap<Vec<&'a str>, Type<'a>>,
    keys: &'b [&'a str],
    expr: &'a Expr,
) -> Result<(), &'c str> {
    let type_: Type = match expr {
        Expr::Ident(ident) => {
            let type_: Type = match types.get(&vec![*ident]) {
                Some(type_) => type_.clone(),
                None => return Err(UNKNOWN_IDENT),
            };
            let _: Option<_> = types.insert(keys.to_vec(), type_);
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
                set_expr(types, &key, &prop.value)?;
                let type_: Type = if let Some(type_) = types.get(&key) {
                    type_.clone()
                } else {
                    unreachable!();
                };
                if let Some(_) = type_props.insert(prop.key, type_) {
                    unreachable!();
                }
            }
            Type::Obj(Rc::new(type_props))
        }
        _ => panic!(format!("{:#?}", expr)),
    };
    match types.insert(keys.to_vec(), type_) {
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
                if let Err(message) = set_expr(&mut types, &key, expr) {
                    return Err(Error { syntax, message });
                }
            }
            _ => panic!(format!("{:#?}", syntax)),
        }
    }
    Ok(types)
}
