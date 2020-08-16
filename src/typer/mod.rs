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
    r#type: Type<'a>,
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
    Ref(Key<'a>),
}

fn get_props<'a>(props: &'a [Prop]) -> Option<Type<'a>> {
    let mut obj: BTreeMap<&str, Type> = BTreeMap::new();
    for prop in props {
        if let Some(r#type) = get_type(&prop.value) {
            let _: Option<Type> = obj.insert(prop.key, r#type);
        } else {
            return None;
        }
    }
    Some(Type::Obj(obj))
}

fn get_array<'a>(exprs: &'a [Expr]) -> Option<Type<'a>> {
    let mut types: BTreeSet<Type> = BTreeSet::new();
    for expr in exprs {
        if let Some(r#type) = get_type(expr) {
            let _: bool = types.insert(r#type);
        } else {
            return None;
        }
    }
    Some(Type::Array(types))
}

fn get_ref<'a>(r#ref: &'a Expr<'a>) -> Option<Key<'a>> {
    match r#ref {
        Expr::Ref(ident) => Some(Key::Var(ident)),
        Expr::Infix { op: ".", left, right } => {
            match (get_ref(left), get_ref(right)) {
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
        Expr::Ref(_) | Expr::Infix { .. } => {
            if let Some(key) = get_ref(expr) {
                Some(Type::Ref(key))
            } else {
                None
            }
        }
        _ => None,
    }
}

macro_rules! deref_obj {
    ($obj:expr, $keys:expr $(,)?) => {{
        let mut value: Option<&Type> = None;
        while !$keys.is_empty() {
            value = $obj.get(&$keys.remove(0));
            if let Some(Type::Obj(next_obj)) = value {
                $obj = next_obj;
            } else {
                break;
            }
        }
        value
    }};
}

fn match_obj<'a>(
    types: &HashMap<Key<'a>, Value<'a>>,
    mut obj: &BTreeMap<&'a str, Type<'a>>,
    idents: &mut Vec<&'a str>,
    r#type: &Type<'a>,
) -> bool {
    let obj_type: Option<&Type> = deref_obj!(obj, idents);
    if let Some(obj_type) = obj_type {
        if let Type::Ref(key) = r#type {
            if let Some(value) = types.get(key) {
                return &value.r#type == obj_type;
            }
        } else {
            return r#type == obj_type;
        }
    }
    false
}

fn set_assign<'a>(
    types: &mut HashMap<Key<'a>, Value<'a>>,
    r#ref: &Expr<'a>,
    expr: &Expr<'a>,
) -> bool {
    match (get_ref(r#ref), get_type(expr)) {
        (Some(key @ Key::Var(_)), Some(Type::Ref(Key::Prop(mut idents)))) => {
            if let Some(value) = types.get(&key) {
                let ident: &str = idents.remove(0);
                if let Some(Value { r#type: Type::Obj(obj), .. }) =
                    types.get(&Key::Var(ident))
                {
                    if match_obj(&types, obj, &mut idents, &value.r#type) {
                        return true;
                    }
                }
            }
        }
        (
            Some(Key::Prop(mut left_idents)),
            Some(Type::Ref(Key::Prop(mut right_idents))),
        ) => {
            let left_ident: &str = left_idents.remove(0);
            let right_ident: &str = right_idents.remove(0);
            if let (
                Some(Value { r#type: Type::Obj(left_obj), .. }),
                Some(Value { r#type: Type::Obj(right_obj), .. }),
            ) = (
                types.get(&Key::Var(left_ident)),
                types.get(&Key::Var(right_ident)),
            ) {
                let mut iter_left_obj = left_obj;
                let mut iter_right_obj = right_obj;
                if deref_obj!(iter_left_obj, left_idents)
                    == deref_obj!(iter_right_obj, right_idents)
                {
                    return true;
                }
            }
        }
        (Some(key @ Key::Var(_)), Some(r#type)) => {
            if let Some(value) = types.get(&key) {
                if r#type == value.r#type {
                    return true;
                }
            }
        }
        (Some(Key::Prop(mut idents)), Some(r#type)) => {
            let ident: &str = idents.remove(0);
            if let Some(Value { r#type: Type::Obj(obj), .. }) =
                types.get(&Key::Var(ident))
            {
                if match_obj(&types, obj, &mut idents, &r#type) {
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
                if let Some(r#type) = get_type(expr) {
                    let key: Key = Key::Var(ident);
                    if !types.contains_key(&key) {
                        let _: Option<Value> = types.insert(
                            key,
                            Value {
                                scope: Vec::new(),
                                r#type,
                                line: syntax.line,
                            },
                        );
                        continue;
                    }
                }
            }
            Stmt::Assign { op: "=", r#ref, expr }
                if set_assign(&mut types, r#ref, expr) =>
            {
                continue;
            }
            _ => (),
        }
        panic!("\n{:#?}\n{:#?}\n", syntax, types)
    }
    types
}
