use crate::{
    ast::*,
    ir::*,
    parser::parse_str_program,
};
use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
    fs::read_to_string
};

// types used for Scope representation
#[derive(Clone)]
enum ConstVal { Int(i64), Float(f64), }
#[derive(Clone)]
enum ScopeItem<'a> {
    Var(&'a str, IRType), // raw_name, type
    Const(ConstVal), // value
    Proc(&'a str, Vec<IRType>, IRType), // raw_name, arg types, return type
    None, // used as a return value
}

struct Scope<'a> {
    contents: HashMap<&'a str, ScopeItem<'a>>,
    namespaces: HashMap<&'a str, Scope<'a>>,
}

// cache for already processed files imported at multiple places
static FILE_CACHE: OnceLock<HashMap<&str, Scope>> = OnceLock::new();
fn get_file_cache<'a>() -> &'static HashMap<&'a str, Scope<'a>> {
    FILE_CACHE.get_or_init(|| HashMap::new())
}

// flag to signal whether compilation finished successfully or not
// don't join files if errors found
pub static mut FOUND_ERRORS: bool = false;


pub fn analyse_and_compile<'a>(source_name: &'a str) -> HashSet<&'a str>
{
    let source = read_to_string(source_name).expect("Failed to read source file");
    let ast = parse_str_program(&source).expect("Failed to parse source file");

    let output_files = HashSet::new();
    let _scope = Scope {
        contents: HashMap::new(),
        namespaces: HashMap::new(),
    };

    // needs to process statements in specific order
    let mut imports_to_process: Vec<TopLevel> = vec![];
    let mut vars_to_process:    Vec<TopLevel> = vec![];
    let mut procs_to_process:   Vec<TopLevel> = vec![];
    // consts processed right away

    for top_level in ast {
        match top_level {
            TopLevel::Import(_, _, _) => imports_to_process.push(top_level),
            TopLevel::VarDecl(_, _) => vars_to_process.push(top_level),
            TopLevel::ProcDecl(_, _, _, _, _, _) => procs_to_process.push(top_level),
            TopLevel::ConstDecl(_nmspc, decls) => {
                for (_ident, _expr) in decls {
                }
            }
        }
    }
    
    output_files
}


fn search_in_scope<'a>(scope: &Scope<'a>, ident: Ident<'a>) -> (ScopeItem<'a>, bool) {
    let mut localp = true;
    let mut current_scope = scope;
    for idnt in ident.namespace {
        match current_scope.namespaces.get(idnt) {
            Some(ns) => {
                current_scope = &ns;
                localp = false;
            },
            None => return (ScopeItem::None, false)
        }
    }
    if let Some(var) = current_scope.contents.get(ident.name) {
        (var.clone(), localp)
    } else {
        (ScopeItem::None, false)
    }
}


// TODO: handle errors somehow
fn expr2ir<'a>(expr: Expr<'a>, scope: &Scope<'a>, expects: IRType) -> IRList<'a> {
    match expr {
        // LITERALS
        Expr::Lit(lit) => match lit {
            Literal::Int(x) => match expects {
                IRType::Int | IRType::I64 => vec![(IRType::I64, IR::LitInt(x))],
                IRType::I32 => vec![(IRType::I32, IR::LitInt(x))],
                IRType::Float | IRType::F64 => vec![(IRType::F64, IR::LitFloat(x as f64))],
                IRType::F32 => vec![(IRType::F32, IR::LitFloat(x as f64))],
                _ => vec![(IRType::Error, IR::Error)]
            },
            Literal::Float(x) => match expects {
                IRType::Float | IRType::F64 => vec![(IRType::F64, IR::LitFloat(x))],
                IRType::F32 => vec![(IRType::F32, IR::LitFloat(x))],
                _ => vec![(IRType::Error, IR::Error)]
            },
            // TODO: handle strings like a normal person
            Literal::Str(s) => match expects {
                IRType::Int | IRType::I32 => vec![(IRType::I32, IR::LitStr(s))],
                IRType::I64 => vec![(IRType::I64, IR::LitStr(s))],
                _ => vec![(IRType::Error, IR::Error)]
            },
        },

        // UNOPS
        Expr::Unop(op, inner) => {
            let mut inner_ir = expr2ir(*inner, scope, expects);
            let inner_type   = irlist_type(&inner_ir);
            match op {
                Unop::Not => inner_ir.extend(vec![(inner_type, IR::Not)]),
                // TODO: do unop at compiletime with literals
                Unop::Neg => inner_ir.extend(vec![(inner_type, IR::Neg)]),
            }
            inner_ir
        }

        // IDENTS
        Expr::Ident(idnt) => {
            let (val, localp) = search_in_scope(&scope, idnt);
            match val {
                // constants -> place as literals
                ScopeItem::Const(val) => match val {
                    ConstVal::Int(x) => expr2ir(Expr::Lit(Literal::Int(x)), scope, expects),
                    ConstVal::Float(x) => expr2ir(Expr::Lit(Literal::Float(x)), scope, expects),
                },

                // variables -> place and cast if needed
                ScopeItem::Var(raw_name, var_type) => {
                    let expr = if localp {
                        (var_type, IR::LocalGet(raw_name))
                    } else {
                        (var_type, IR::GlobalGet(raw_name))
                    };
                    ir_resolve_types(expr, expects)
                },

                // proc/not found -> error
                _ => {
                    vec![(IRType::Error, IR::Error)]
                },
            }
        }

        // BINOPS
        // left side determines result type
        // Expr::Binop(op, left, right) => {
        //     let mut left_ir = expr2ir(*left, scope.clone(), expects);
        //     let left_type = irlist_type(&left_ir);
        //     let mut right_ir = expr2ir(*right, scope, expects);
        //     let right_type = irlist_type(&right_ir);
        // }

        _ => vec![(IRType::Error, IR::Error)]
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_SCOPE: OnceLock<Scope> = OnceLock::new();
    fn get_test_scope<'a>() -> &'static Scope<'a> {
        TEST_SCOPE.get_or_init(|| {
            let mut scope = Scope {
                contents: HashMap::new(),
                namespaces: HashMap::new(),
            };
            scope.contents.insert("x", ScopeItem::Var("raw_x", IRType::I32));
            scope.contents.insert("y", ScopeItem::Const(ConstVal::Int(42)));

            let mut nmspc_scope = Scope {
                contents: HashMap::new(),
                namespaces: HashMap::new(),
            };
            nmspc_scope.contents.insert("z", ScopeItem::Var("raw_z", IRType::F64));
            nmspc_scope.contents.insert("w", ScopeItem::Const(ConstVal::Float(3.7)));
            scope.namespaces.insert("n", nmspc_scope);

            scope
        })
    }

    #[test]
    fn test_expr2ir() {
        assert_eq!(
            expr2ir(
                Expr::Unop(Unop::Neg, Box::new(
                        Expr::Ident(Ident {name: "w", namespace: vec!["n"]}))),
                get_test_scope(), IRType::F32),
            vec![
                (IRType::F32, IR::LitFloat(3.7)),
                (IRType::F32, IR::Neg)
            ]
        );
        assert_eq!(
            expr2ir(
                Expr::Unop(Unop::Neg, Box::new(
                        Expr::Ident(Ident {name: "wZ", namespace: vec!["n"]}))),
                get_test_scope(), IRType::F32),
            vec![
                (IRType::Error, IR::Error),
                (IRType::Error, IR::Neg)
            ]
        )
    }
}
