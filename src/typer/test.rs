use super::{get_types, Error, Message};
use crate::btree_map;
use crate::commenter::{get_sigs, Comment};
use crate::parser::{get_ast, Expr, Prop, Stmt, Syntax};
use crate::tokenizer::{get_tokens, Asn, Op};
use crate::types::{Target, Type};
use std::collections::{BTreeMap, HashMap};

macro_rules! assert_types {
    ($a:expr, $b:expr $(,)?) => {{
        let (ast, comments): (Vec<Syntax>, Vec<Comment>) =
            get_ast(&get_tokens($a)).unwrap();
        let mut sigs: HashMap<Target, Type> = get_sigs(&comments).unwrap();
        assert_eq!(get_types(&ast, &mut sigs), $b)
    }};
}

#[test]
fn declares() {
    assert_types!(
        "var a = \"?\";
         var b = 0;
         var c = true;
         var d = null;
         var e = undefined;",
        Ok(btree_map![
            (Target { ident: vec!["a"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["b"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["c"], scope: Vec::new() }, Type::Bool),
            (Target { ident: vec!["d"], scope: Vec::new() }, Type::Null),
            (Target { ident: vec!["e"], scope: Vec::new() }, Type::Undef),
        ]),
    )
}

#[test]
fn declare_shadow_ident() {
    assert_types!(
        "var x = true;
         var x = false;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Bool("false"),
                },
                line: 1,
            },
            message: Message::IdentShadow,
        }),
    )
}

#[test]
fn declare_unknown_ident() {
    assert_types!(
        "var y = x;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl { ident: "y", expr: Expr::Ident("x") },
                line: 0,
            },
            message: Message::IdentUnknown,
        }),
    )
}

#[test]
fn declare_ident() {
    assert_types!(
        "var x = 0;
         var y = x;",
        Ok(btree_map![
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Num),
        ]),
    )
}

#[test]
fn declare_object() {
    assert_types!(
        "var x = 0;
         var y = { a: x };",
        Ok(btree_map![
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["y", "a"], scope: Vec::new() }, Type::Num),
            (
                Target { ident: vec!["y"], scope: Vec::new() },
                Type::Obj(btree_map![("a", Type::Num)]),
            ),
        ]),
    )
}

#[test]
fn declare_empty_object() {
    assert_types!(
        "var x = {};",
        Ok(btree_map![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::Obj(BTreeMap::new()),
        )]),
    )
}

#[test]
fn declare_nested_object() {
    let props: BTreeMap<&str, Type> = btree_map![("a", Type::Num)];
    assert_types!(
        "var x = {
             a: 0,
             b: \"?\",
             c: true,
             d: null,
             e: undefined,
             f: {
                 a: 0,
             },
         };",
        Ok(btree_map![
            (Target { ident: vec!["x", "a"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["x", "b"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["x", "c"], scope: Vec::new() }, Type::Bool),
            (Target { ident: vec!["x", "d"], scope: Vec::new() }, Type::Null),
            (Target { ident: vec!["x", "e"], scope: Vec::new() }, Type::Undef),
            (
                Target { ident: vec!["x", "f", "a"], scope: Vec::new() },
                Type::Num,
            ),
            (
                Target { ident: vec!["x", "f"], scope: Vec::new() },
                Type::Obj(props.clone()),
            ),
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Obj(btree_map![
                    ("a", Type::Num),
                    ("b", Type::Str),
                    ("c", Type::Bool),
                    ("d", Type::Null),
                    ("e", Type::Undef),
                    ("f", Type::Obj(props)),
                ]),
            ),
        ]),
    )
}

#[test]
fn declare_object_duplicate_keys() {
    assert_types!(
        "var x = {
             a: true,
             a: false,
         };",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Obj(vec![
                        Prop { key: "a", value: Expr::Bool("true") },
                        Prop { key: "a", value: Expr::Bool("false") },
                    ]),
                },
                line: 0,
            },
            message: Message::ObjDuplicateKeys,
        }),
    )
}

#[test]
fn declare_array() {
    assert_types!(
        "var x = [0, 1, 2, 3];",
        Ok(btree_map![
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Array(Box::new(Type::Num)),
            ),
            (
                Target { ident: vec!["x", "push"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![Type::Num], Type::Undef)]),
            ),
        ]),
    )
}

#[test]
fn declare_array_empty() {
    assert_types!(
        "var x = [];",
        Ok(btree_map![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::EmptyArray,
        )]),
    )
}

#[test]
fn declare_array_obj() {
    let obj: Type = Type::Obj(btree_map![("x", Type::Num)]);
    assert_types!(
        "var a = {
             x: 0,
         };
         var b = {
             x: 1,
         };
         var xs = [a, b];",
        Ok(btree_map![
            (Target { ident: vec!["a", "x"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["a"], scope: Vec::new() }, obj.clone()),
            (Target { ident: vec!["b", "x"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["b"], scope: Vec::new() }, obj.clone()),
            (
                Target { ident: vec!["xs"], scope: Vec::new() },
                Type::Array(Box::new(obj.clone())),
            ),
            (
                Target { ident: vec!["xs", "push"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![obj], Type::Undef)]),
            ),
        ]),
    )
}

#[test]
fn declare_array_err() {
    assert_types!(
        "var x = [0, \"0\"];",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Array(vec![Expr::Num("0"), Expr::Str("0")]),
                },
                line: 0,
            },
            message: Message::ArrayMultiType,
        }),
    )
}

#[test]
fn declare_uninit_ident_err() {
    assert_types!(
        "var x;
         var y = x;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl { ident: "y", expr: Expr::Ident("x") },
                line: 1,
            },
            message: Message::IdentUninit,
        }),
    )
}

#[test]
fn declare_prefix() {
    assert_types!(
        "var a = !false;
         var b = -0;
         var c = ~0;
         var d = ++b;
         var e = --c;",
        Ok(btree_map![
            (Target { ident: vec!["a"], scope: Vec::new() }, Type::Bool),
            (Target { ident: vec!["b"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["c"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["d"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["e"], scope: Vec::new() }, Type::Num),
        ]),
    )
}

#[test]
fn declare_prefix_err() {
    assert_types!(
        "var a = null;
         var b = !a;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "b",
                    expr: Expr::Prefix {
                        op: Op::Not,
                        expr: Box::new(Expr::Ident("a")),
                    }
                },
                line: 1,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn declare_infix_member() {
    let props: BTreeMap<&str, Type> = btree_map![("y", Type::Num)];
    assert_types!(
        "var a = {
             x: {
                 y: 0,
             },
         };
         var x = null;
         var y = \"\";
         var b = a.x.y;",
        Ok(btree_map![
            (
                Target { ident: vec!["a", "x", "y"], scope: Vec::new() },
                Type::Num,
            ),
            (
                Target { ident: vec!["a", "x"], scope: Vec::new() },
                Type::Obj(props.clone()),
            ),
            (
                Target { ident: vec!["a"], scope: Vec::new() },
                Type::Obj(btree_map![("x", Type::Obj(props))]),
            ),
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Null),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["b"], scope: Vec::new() }, Type::Num),
        ]),
    )
}

#[test]
fn declare_infix_member_err() {
    assert_types!(
        "var a = {
             x: {
                 y: 0,
             },
         };
         var b = a.\"x\".y;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "b",
                    expr: Expr::Infix {
                        op: Op::Member,
                        left: Box::new(Expr::Infix {
                            op: Op::Member,
                            left: Box::new(Expr::Ident("a")),
                            right: Box::new(Expr::Str("x")),
                        }),
                        right: Box::new(Expr::Ident("y")),
                    },
                },
                line: 5,
            },
            message: Message::NonIdentMember,
        }),
    )
}

#[test]
fn declare_update() {
    assert_types!(
        "var x = 0;
         x = 1;",
        Ok(btree_map![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::Num
        )]),
    )
}

#[test]
fn declare_update_err() {
    assert_types!(
        "var x = 0;
         x = \"?\";",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Ident("x"),
                    expr: Expr::Str("?"),
                },
                line: 1,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn declare_uninit() {
    assert_types!(
        "var x;
         x = 1;
         x = 2;",
        Ok(btree_map![(
            Target { ident: vec!["x"], scope: Vec::new() },
            Type::Num
        )]),
    )
}

#[test]
fn declare_uninit_err() {
    assert_types!(
        "var x;
         x = \"?\";
         x = 2;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Ident("x"),
                    expr: Expr::Num("2"),
                },
                line: 2,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn assign_err() {
    assert_types!(
        "var x;
         \"x\" = 0;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Str("x"),
                    expr: Expr::Num("0"),
                },
                line: 1,
            },
            message: Message::AssignNonIdent,
        }),
    )
}

#[test]
fn assign_obj() {
    assert_types!(
        "var x = { a: 0 };
         var y = 1;
         x.a = y;",
        Ok(btree_map![
            (Target { ident: vec!["x", "a"], scope: Vec::new() }, Type::Num),
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Obj(btree_map![("a", Type::Num)]),
            ),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Num),
        ]),
    )
}

#[test]
fn assign_obj_type_err() {
    assert_types!(
        "var x = { a: 0 };
         x.a = \"0\";",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Infix {
                        op: Op::Member,
                        left: Box::new(Expr::Ident("x")),
                        right: Box::new(Expr::Ident("a")),
                    },
                    expr: Expr::Str("0"),
                },
                line: 1,
            },
            message: Message::IncompatibleTypes,
        }),
    )
}

#[test]
fn assign_obj_key_err() {
    assert_types!(
        "var x = { a: 0 };
         x.b = 1;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Infix {
                        op: Op::Member,
                        left: Box::new(Expr::Ident("x")),
                        right: Box::new(Expr::Ident("b")),
                    },
                    expr: Expr::Num("1"),
                },
                line: 1,
            },
            message: Message::IdentUnknown,
        }),
    )
}

#[test]
fn assign_uninit_ident_err() {
    assert_types!(
        "var x;
         var y;
         y = x;",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Assign {
                    op: Asn::Reg,
                    ident: Expr::Ident("y"),
                    expr: Expr::Ident("x"),
                },
                line: 2,
            },
            message: Message::IdentUninit,
        }),
    )
}

#[test]
fn access() {
    assert_types!(
        "var xs = [1];
         var x = xs[0];",
        Ok(btree_map![
            (
                Target { ident: vec!["xs"], scope: Vec::new() },
                Type::Array(Box::new(Type::Num)),
            ),
            (
                Target { ident: vec!["xs", "push"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![Type::Num], Type::Undef)]),
            ),
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Num),
        ]),
    )
}

#[test]
fn access_non_array_err() {
    assert_types!(
        "var xs = \"[1]\";
         var x = xs[0];",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Access {
                        expr: Box::new(Expr::Ident("xs")),
                        index: Box::new(Expr::Num("0")),
                    },
                },
                line: 1,
            },
            message: Message::AccessNonArray,
        }),
    )
}

#[test]
fn access_non_index_err() {
    assert_types!(
        "var xs = [1];
         var x = xs[\"0\"];",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Decl {
                    ident: "x",
                    expr: Expr::Access {
                        expr: Box::new(Expr::Ident("xs")),
                        index: Box::new(Expr::Str("0")),
                    },
                },
                line: 1,
            },
            message: Message::AccessNonIndex,
        }),
    )
}

#[test]
fn declare_functions() {
    let obj: Type = Type::Obj(btree_map![("a", Type::Bool)]);
    assert_types!(
        "/* x { a: bool }
          * f(number, string) -> number
          * f(string, bool) -> string
          * f(string, null) -> string
          * f(x, undefined) -> x
          * g(number) -> number
          * g(string) -> number
          * g(x) -> number
          */

         var x = \"???\";
         var y = null;

         function f(x, _) {
             return x;
         }

         function g(_) {
             var y = 0;
             return y;
         }

         /* h(x) -> bool */
         function h(x) {
             return x.a;
         }

         // i(number) -> undefined
         // i(string) -> undefined
         // i(null) -> undefined
         // i(bool) -> undefined
         // i(undefined) -> undefined
         function i(_) {}",
        Ok(btree_map![
            (Target { ident: vec!["x"], scope: Vec::new() }, Type::Str),
            (Target { ident: vec!["y"], scope: Vec::new() }, Type::Null),
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(btree_map![
                    (vec![Type::Num, Type::Str], Type::Num),
                    (vec![Type::Str, Type::Bool], Type::Str),
                    (vec![Type::Str, Type::Null], Type::Str),
                    (vec![obj.clone(), Type::Undef], obj.clone()),
                ]),
            ),
            (
                Target { ident: vec!["g"], scope: Vec::new() },
                Type::Fn(btree_map![
                    (vec![Type::Num], Type::Num),
                    (vec![Type::Str], Type::Num),
                    (vec![obj.clone()], Type::Num),
                ]),
            ),
            (
                Target { ident: vec!["h"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![obj], Type::Bool)]),
            ),
            (
                Target { ident: vec!["i"], scope: Vec::new() },
                Type::Fn(btree_map![
                    (vec![Type::Num], Type::Undef),
                    (vec![Type::Str], Type::Undef),
                    (vec![Type::Null], Type::Undef),
                    (vec![Type::Bool], Type::Undef),
                    (vec![Type::Undef], Type::Undef),
                ]),
            ),
        ]),
    )
}

#[test]
fn declare_scoped_function() {
    let obj: Type = Type::Obj(btree_map![("a", Type::Num)]);
    assert_types!(
        "/* x {
          *     a: number,
          * }
          * f(x) -> number
          * h(x) -> undefined
          * h @ g() -> undefined
          * h @ g @ f() -> number
          */

         function f(x) {
             return x.a;
         }

         function h(x) {
             function g() {
                 function f() {
                     return x.a;
                 }
             }
         }",
        Ok(btree_map![
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![obj.clone()], Type::Num)]),
            ),
            (
                Target { ident: vec!["h"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![obj.clone()], Type::Undef)]),
            ),
            (
                Target { ident: vec!["g"], scope: vec!["h"] },
                Type::Fn(btree_map![(Vec::new(), Type::Undef)]),
            ),
            (
                Target { ident: vec!["f"], scope: vec!["h", "g"] },
                Type::Fn(btree_map![(Vec::new(), Type::Num)]),
            ),
        ]),
    )
}

#[test]
fn nested_function_returns() {
    let obj: Type = Type::Obj(btree_map![("a", Type::Num)]);
    let fn_: Type = Type::Fn(btree_map![(Vec::new(), Type::Num)]);
    assert_types!(
        "// x {
         //     a: number,
         // }
         // h(x) -> number
         // h @ g() -> number
         // h @ g @ f() -> number

         function h(x) {
             function g() {
                 function f() {
                     return x.a;
                 }
                 return f();
             }
             return g();
         }",
        Ok(btree_map![
            (
                Target { ident: vec!["h"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![obj.clone()], Type::Num)]),
            ),
            (Target { ident: vec!["g"], scope: vec!["h"] }, fn_.clone()),
            (Target { ident: vec!["f"], scope: vec!["h", "g"] }, fn_),
        ]),
    )
}

#[test]
fn return_closure() {
    let obj: Type = Type::Obj(btree_map![("a", Type::Num)]);
    let fn_: Type = Type::Fn(btree_map![(Vec::new(), Type::Num)]);
    assert_types!(
        "/*  x {
          *      a: number,
          *  }
          *  h(x) -> () -> () -> number
          *  h @ g() -> () -> number
          *  h @ g @ f() -> number
          */

         function h(x) {
             function g() {
                 function f() {
                     return x.a;
                 }
                 return f;
             }
             return g;
         }",
        Ok(btree_map![
            (
                Target { ident: vec!["h"], scope: Vec::new() },
                Type::Fn(btree_map![(
                    vec![obj],
                    Type::Fn(btree_map![(Vec::new(), fn_.clone())]),
                )]),
            ),
            (
                Target { ident: vec!["g"], scope: vec!["h"] },
                Type::Fn(btree_map![(Vec::new(), fn_.clone())]),
            ),
            (Target { ident: vec!["f"], scope: vec!["h", "g"] }, fn_),
        ]),
    )
}

#[test]
fn assign_closures() {
    let obj: Type = Type::Obj(btree_map![("a", Type::Num)]);
    let fn_: Type = Type::Fn(btree_map![(Vec::new(), Type::Num)]);
    assert_types!(
        "// x {
         //     a: number,
         // }

         // f(x) -> number
         function f(x) {
             return x.a;
         }

         // g(x) -> () -> number
         // g @ f() -> number
         function g(x) {
             function f() {
                 return x.a;
             }
             return f;
         }

         var a = { a: 0 };
         var b = { b: a };
         var c = f(b.b);
         var d = g(b.b);
         var e = 0;
         e = c;
         e = d();",
        Ok(btree_map![
            (
                Target { ident: vec!["f"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![obj.clone()], Type::Num)]),
            ),
            (
                Target { ident: vec!["g"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![obj.clone()], fn_.clone())]),
            ),
            (Target { ident: vec!["f"], scope: vec!["g"] }, fn_.clone()),
            (Target { ident: vec!["a"], scope: Vec::new() }, obj.clone()),
            (Target { ident: vec!["a", "a"], scope: Vec::new() }, Type::Num),
            (
                Target { ident: vec!["b"], scope: Vec::new() },
                Type::Obj(btree_map![("b", obj.clone())]),
            ),
            (Target { ident: vec!["b", "b"], scope: Vec::new() }, obj),
            (
                Target { ident: vec!["b", "b", "a"], scope: Vec::new() },
                Type::Num,
            ),
            (Target { ident: vec!["c"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["d"], scope: Vec::new() }, fn_),
            (Target { ident: vec!["e"], scope: Vec::new() }, Type::Num),
        ]),
    )
}

#[test]
fn replaced_binding_err() {
    assert_types!(
        "var x = {
             a: 0,
             b: \"?\",
         };

         // g() -> () -> string
         // g @ f() -> string
         function g() {
             var x = null;
             function f() {
                 return x.b;
             }
             return f;
         }",
        Err(Error {
            syntax: &Syntax {
                statement: Stmt::Ret(Expr::Infix {
                    op: Op::Member,
                    left: Box::new(Expr::Ident("x")),
                    right: Box::new(Expr::Ident("b")),
                }),
                line: 10,
            },
            message: Message::IdentUnknown,
        }),
    )
}

#[test]
fn replaced_binding_ok() {
    let fn_: Type = Type::Fn(btree_map![(Vec::new(), Type::Num)]);
    assert_types!(
        "var x = {
             a: 0,
             b: \"?\",
         };

         // g() -> () -> number
         // g @ f() -> number
         function g() {
             var x = {
                 b: 0,
             };
             function f() {
                 var b = x.b;
                 return x.b;
             }
             return f;
         }",
        Ok(btree_map![
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Obj(btree_map![("a", Type::Num), ("b", Type::Str)]),
            ),
            (Target { ident: vec!["x", "a"], scope: Vec::new() }, Type::Num),
            (Target { ident: vec!["x", "b"], scope: Vec::new() }, Type::Str),
            (
                Target { ident: vec!["f"], scope: vec!["g"] },
                Type::Fn(btree_map![(Vec::new(), Type::Num)]),
            ),
            (
                Target { ident: vec!["g"], scope: Vec::new() },
                Type::Fn(btree_map![(Vec::new(), fn_.clone())]),
            ),
            (Target { ident: vec!["f"], scope: vec!["g"] }, fn_),
        ]),
    )
}

#[test]
fn array_push() {
    let array: Type = Type::Array(Box::new(Type::Num));
    let obj_z: Type = Type::Obj(btree_map![("z", array.clone())]);
    let obj_y: Type = Type::Obj(btree_map![("y", obj_z.clone())]);
    assert_types!(
        "var x = {
             y: {
                 z: [0],
             },
         };
         x.y.z.push(0);",
        Ok(btree_map![
            (Target { ident: vec!["x"], scope: Vec::new() }, obj_y),
            (Target { ident: vec!["x", "y"], scope: Vec::new() }, obj_z),
            (Target { ident: vec!["x", "y", "z"], scope: Vec::new() }, array),
            (
                Target {
                    ident: vec!["x", "y", "z", "push"],
                    scope: Vec::new(),
                },
                Type::Fn(btree_map![(vec![Type::Num], Type::Undef)]),
            ),
        ]),
    )
}

#[test]
fn fn_in_obj() {
    let array: Type = Type::Array(Box::new(Type::Num));
    let fn_: Type = Type::Fn(btree_map![(Vec::new(), array.clone())]);
    assert_types!(
        "/*
          *  f() -> [number]
          *  x {
          *      f: () -> [number]
          *  }
          */

         function f() {
             return [0];
         }

         var x = {
             f: f,
         };
         var xs = x.f();
         xs.push(0);",
        Ok(btree_map![
            (
                Target { ident: vec!["x"], scope: Vec::new() },
                Type::Obj(btree_map![("f", fn_.clone())]),
            ),
            (Target { ident: vec!["f"], scope: Vec::new() }, fn_.clone()),
            (Target { ident: vec!["x", "f"], scope: Vec::new() }, fn_),
            (Target { ident: vec!["xs"], scope: Vec::new() }, array),
            (
                Target { ident: vec!["xs", "push"], scope: Vec::new() },
                Type::Fn(btree_map![(vec![Type::Num], Type::Undef)]),
            ),
        ]),
    )
}
