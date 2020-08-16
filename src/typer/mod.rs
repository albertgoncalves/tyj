#[cfg(test)]
mod test;

use crate::parser::{Expr, Prop, Stmt, Syntax};
use crate::tokenizer::Count;
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) enum Key<'a> {
    Var(&'a str),
    Prop(Vec<&'a str>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Value<'a> {
    scope: Vec<&'a str>,
    type_: Type<'a>,
    line: Count,
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum Type<'a> {
    Num,
    Str,
    Bool,
    Null,
    Undef,
    Obj(BTreeMap<&'a str, Type<'a>>),
    Array(BTreeSet<Type<'a>>),
    Ident(Key<'a>),
}

fn get_props<'a>(parse_props: &'a [Prop]) -> Option<Type<'a>> {
    let mut type_props: BTreeMap<&str, Type> = BTreeMap::new();
    for prop in parse_props {
        if let Some(type_) = get_type(&prop.value) {
            let _: Option<Type> = type_props.insert(prop.key, type_);
        } else {
            return None;
        }
    }
    Some(Type::Obj(type_props))
}

fn get_array<'a>(exprs: &'a [Expr]) -> Option<Type<'a>> {
    let mut types: BTreeSet<Type> = BTreeSet::new();
    for expr in exprs {
        if let Some(type_) = get_type(expr) {
            let _: bool = types.insert(type_);
        } else {
            return None;
        }
    }
    Some(Type::Array(types))
}

fn get_ident<'a>(ident: &'a Expr<'a>) -> Option<Key<'a>> {
    match ident {
        Expr::Ident(ident) => Some(Key::Var(ident)),
        Expr::Infix { op: ".", left, right } => {
            match (get_ident(left), get_ident(right)) {
                (Some(Key::Prop(mut left)), Some(Key::Prop(mut right))) => {
                    left.append(&mut right);
                    Some(Key::Prop(left))
                }
                (Some(Key::Prop(mut left)), Some(Key::Var(right))) => {
                    left.push(right);
                    Some(Key::Prop(left))
                }
                (Some(Key::Var(left)), Some(Key::Prop(mut right))) => {
                    let mut left: Vec<&str> = vec![left];
                    left.append(&mut right);
                    Some(Key::Prop(left))
                }
                (Some(Key::Var(left)), Some(Key::Var(right))) => {
                    let mut left: Vec<&str> = vec![left];
                    left.push(right);
                    Some(Key::Prop(left))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn get_type<'a>(expr: &'a Expr<'a>) -> Option<Type<'a>> {
    match expr {
        Expr::Num(_) => Some(Type::Num),
        Expr::Str(_) => Some(Type::Str),
        Expr::Bool(_) => Some(Type::Bool),
        Expr::Null => Some(Type::Null),
        Expr::Undef => Some(Type::Undef),
        Expr::Obj(x) => get_props(x),
        Expr::Array(x) => get_array(x),
        Expr::Ident(_) | Expr::Infix { .. } => {
            if let Some(x) = get_ident(expr) {
                Some(Type::Ident(x))
            } else {
                None
            }
        }
        _ => None,
    }
}

macro_rules! deref_obj {
    ($props:expr, $idents:expr $(,)?) => {{
        let mut value: Option<&Type> = None;
        while !$idents.is_empty() {
            value = $props.get(&$idents.remove(0));
            if let Some(Type::Obj(next_props)) = value {
                $props = next_props;
            } else {
                break;
            }
        }
        value
    }};
}

fn match_obj<'a>(
    types: &HashMap<Key<'a>, Value<'a>>,
    type_: &Type<'a>,
    mut props: &BTreeMap<&'a str, Type<'a>>,
    idents: &mut Vec<&'a str>,
) -> bool {
    let obj_type: Option<&Type> = deref_obj!(props, idents);
    if let Some(obj_type) = obj_type {
        if let Type::Ident(key) = type_ {
            if let Some(value) = types.get(key) {
                return &value.type_ == obj_type;
            }
        } else {
            return type_ == obj_type;
        }
    }
    false
}

fn set_assign<'a>(
    types: &mut HashMap<Key<'a>, Value<'a>>,
    ident: &Expr<'a>,
    expr: &Expr<'a>,
) -> bool {
    match (get_ident(ident), get_type(expr)) {
        (
            Some(key @ Key::Var(_)),
            Some(Type::Ident(Key::Prop(mut idents))),
        ) => {
            if let Some(value) = types.get(&key) {
                let ident: &str = idents.remove(0);
                if let Some(Value { type_: Type::Obj(props), .. }) =
                    types.get(&Key::Var(ident))
                {
                    if match_obj(&types, &value.type_, props, &mut idents) {
                        return true;
                    }
                }
            }
        }
        (
            Some(Key::Prop(mut left_idents)),
            Some(Type::Ident(Key::Prop(mut right_idents))),
        ) => {
            let left_ident: &str = left_idents.remove(0);
            let right_ident: &str = right_idents.remove(0);
            if let (
                Some(Value { type_: Type::Obj(left_props), .. }),
                Some(Value { type_: Type::Obj(right_props), .. }),
            ) = (
                types.get(&Key::Var(left_ident)),
                types.get(&Key::Var(right_ident)),
            ) {
                let mut iter_left_props = left_props;
                let mut iter_right_props = right_props;
                if deref_obj!(iter_left_props, left_idents)
                    == deref_obj!(iter_right_props, right_idents)
                {
                    return true;
                }
            }
        }
        (Some(key @ Key::Var(_)), Some(type_)) => {
            if let Some(value) = types.get(&key) {
                if type_ == value.type_ {
                    return true;
                }
            }
        }
        (Some(Key::Prop(mut idents)), Some(type_)) => {
            let ident: &str = idents.remove(0);
            if let Some(Value { type_: Type::Obj(props), .. }) =
                types.get(&Key::Var(ident))
            {
                if match_obj(&types, &type_, props, &mut idents) {
                    return true;
                }
            }
        }
        _ => (),
    }
    false
}

pub(crate) fn get_types<'a>(ast: &'a [Syntax]) -> HashMap<Key<'a>, Value<'a>> {
    let mut types: HashMap<Key, Value> = HashMap::new();
    for syntax in ast {
        match &syntax.statement {
            Stmt::Decl { ident, expr } => {
                if let Some(type_) = get_type(expr) {
                    let key: Key = Key::Var(ident);
                    if !types.contains_key(&key) {
                        let _: Option<Value> = types.insert(
                            key,
                            Value {
                                scope: Vec::new(),
                                type_,
                                line: syntax.line,
                            },
                        );
                        continue;
                    }
                }
            }
            Stmt::Assign { op: "=", ident, expr }
                if set_assign(&mut types, ident, expr) =>
            {
                continue;
            }
            _ => (),
        }
        panic!("\n{:#?}\n{:#?}\n", syntax, types)
    }
    types
}
