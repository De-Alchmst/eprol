use crate::{
    ast::*,
    ir::*,
    parser::parse_str_program,
    name_encoding::raw_name,
};
use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
    fs::read_to_string
};

// types used for Scope representation
#[derive(Clone)]
enum LitVal<'a> { Int(i64), Float(f64), Str(&'a str) }
#[derive(Clone)]
enum ScopeItem<'a> {
    Var(String, IRType), // raw_name, type
    Const(LitVal<'a>), // value
    Proc(String, Vec<IRType>, IRType), // raw_name, arg types, return type
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
    let mut scope = Scope {
        contents: HashMap::new(),
        namespaces: HashMap::new(),
    };

    let mut file_ir: TopLevelIRList = vec![];

    // needs to process statements in specific order
    let mut imports_to_process: Vec<TopLevel> = vec![];
    let mut vars_to_process:    Vec<TopLevel> = vec![];
    let mut procs_to_process:   Vec<TopLevel> = vec![];

    // CONST processing
    // and fill other lists for later processing
    for top_level in ast {
        match top_level {
            TopLevel::Import(_, _, _) => imports_to_process.push(top_level),
            TopLevel::VarDecl(_, _) => vars_to_process.push(top_level),
            TopLevel::ProcDecl(_, _, _, _, _, _) => procs_to_process.push(top_level),
            TopLevel::ConstDecl(nmspc, decls) => {
                for (name, expr) in decls {
                    let idnt = Ident {name, namespace: nmspc.clone()};
                    let (val, _localp) = scope.search(&idnt);
                    if let ScopeItem::None = val {
                        let expr_ir = expr2ir(&expr, &scope, IRType::Any);
                        if let Some(lit_val) = irlist_lit(&expr_ir) {
                            scope.insert(&idnt, ScopeItem::Const(lit_val));
                        } else {
                            // TODO: handle error on non-literal const value
                        }
                    } else {
                        // TODO: handle error of redeclaration
                    }
                }
            }
        }
    }


    // IMPORTS
    for top_level in imports_to_process {
        if let TopLevel::Import(outer, inner, typ) = top_level {
            let (val, _localp) = scope.search(&inner);

            if let ScopeItem::None = val {
                let typ = asttype_to_irtype(typ);
                let raw_name = raw_name(&inner, source_name);
                match typ.clone() {
                    IRType::Func(arg_types, ret_type) => {
                        file_ir.push(TopLevelIR::Import(outer, raw_name, typ));
                    },
                    
                    _ => {

                    }
                }
            } else {
                // TODO: handle error of redeclaration
            }
        }
    }

    
    output_files
}


impl Scope<'_> {
    fn search<'a>(&'a self, ident: &Ident<'a>) -> (ScopeItem<'a>, bool) {
        let mut localp = true;
        let mut current_scope = self;

        // step through namespaces to find required scope, return Error if not found
        for i in 0..ident.namespace.len() {
            match current_scope.namespaces.get(ident.namespace[i]) {
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


    fn insert<'a>(&'a mut self, ident: &Ident<'a>, item: ScopeItem) -> bool {
        let mut current_scope = self;

        // step through namespaces to find required scope, return Error if not found
        for i in 0..ident.namespace.len() {
            match current_scope.namespaces.get(ident.namespace[i]) {
                Some(ns) => {
                    current_scope = &mut ns;
                },
                None => {
                    self.namespaces.insert(ident.namespace[i], Scope {
                        contents: HashMap::new(),
                        namespaces: HashMap::new(),
                    });
                }
            }
        }

        if let Some(_) = current_scope.contents.get(ident.name) {
            return false;
        } else {
            self.contents.insert(ident.name, item);
            true
        }
    }
}


fn irlist_lit<'a>(ir: &IRList<'a>) -> Option<LitVal<'a>> {
    match ir.last() {
        Some((_, IR::LitInt(x))) => Some(LitVal::Int(*x)),
        Some((_, IR::LitFloat(x))) => Some(LitVal::Float(*x)),
        Some((_, IR::LitStr(s))) => Some(LitVal::Str(s)),
        _ => None
    }
}


// TODO: handle errors somehow
fn expr2ir<'a>(expr: &Expr<'a>, scope: &'a Scope<'a>, expects: IRType) -> IRList<'a> {
    match expr {
        // LITERALS
        Expr::Lit(lit) => match lit {
            Literal::Int(x) => match expects {
                IRType::Int | IRType::I64 => vec![(IRType::I64, IR::LitInt(*x))],
                IRType::I32 => vec![(IRType::I32, IR::LitInt(*x))],
                IRType::Float | IRType::F64 => vec![(IRType::F64, IR::LitFloat(*x as f64))],
                IRType::F32 => vec![(IRType::F32, IR::LitFloat(*x as f64))],
                _ => vec![(IRType::Error, IR::Error)]
            },
            Literal::Float(x) => match expects {
                IRType::Float | IRType::F64 => vec![(IRType::F64, IR::LitFloat(*x))],
                IRType::F32 => vec![(IRType::F32, IR::LitFloat(*x))],
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
            let mut inner_ir = expr2ir(inner, scope, expects);
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
            let (val, localp) = scope.search(idnt);
            match val {
                // constants -> place as literals
                ScopeItem::Const(val) => match val {
                    LitVal::Int(x) => expr2ir(&Expr::Lit(Literal::Int(x)), scope, expects),
                    LitVal::Float(x) => expr2ir(&Expr::Lit(Literal::Float(x)), scope, expects),
                    LitVal::Str(s) => expr2ir(&Expr::Lit(Literal::Str(s)), scope, expects),
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
        // therefor right must match left and the entire outcome must then
        // match expects
        // TODO:: do binop at compiletime with literals
        // TODO:: add unsigned support for binops
        Expr::Binop(op, left, right) => {
            // evaluate both sides
            let mut left_ir = expr2ir(left, scope, expects.clone());
            let left_type = irlist_type(&left_ir);
            let mut right_ir = expr2ir(right, scope, left_type.clone());
            let right_type = irlist_type(&right_ir);

            // fix right type if needed
            if right_type != left_type {
                let last_right = right_ir.pop().unwrap_or((IRType::Error, IR::Error));
                right_ir.extend(ir_resolve_types(last_right, left_type.clone()));
            }

            left_ir.extend(right_ir);
            left_ir.extend(
                ir_resolve_types((left_type, match op {
                    Binop::Add => IR::Add,
                    Binop::Sub => IR::Sub,
                    Binop::Mul => IR::Mul(false),
                    Binop::Div => IR::Div(false),
                    _ => IR::Error,
                }), expects)
            );
            left_ir
        },



        // PROC CALLS
        Expr::ProcCall(idnt, args) => {
            let (val, _localp) = scope.search(idnt);
            match val {
                ScopeItem::Proc(raw_name, arg_types, ret_type) => {
                    // check args length
                    if arg_types.len() != args.len() {
                        return vec![(IRType::Error, IR::Error)];
                    }

                    // evaluate args to their required types
                    let mut ir = vec![];
                    for i in 0..args.len() {
                        let arg_ir = expr2ir(&args[i], scope, arg_types[i].clone());
                        ir.extend(arg_ir);
                    }
                    ir.extend(ir_resolve_types((ret_type.clone(),
                                                IR::Call(raw_name)), expects));
                    ir
                },

                _ => vec![(IRType::Error, IR::Error)]
            }
        },

        // _ => vec![(IRType::Error, IR::Error)]
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
            scope.contents.insert("x", ScopeItem::Var("raw_x".to_string(), IRType::I32));
            scope.contents.insert("y", ScopeItem::Const(LitVal::Int(42)));
            scope.contents.insert("f", ScopeItem::Proc("raw_f".to_string(), vec![IRType::I32, IRType::F64], IRType::F64));

            let mut nmspc_scope = Scope {
                contents: HashMap::new(),
                namespaces: HashMap::new(),
            };
            nmspc_scope.contents.insert("z", ScopeItem::Var("raw_z".to_string(), IRType::F64));
            nmspc_scope.contents.insert("w", ScopeItem::Const(LitVal::Float(3.7)));
            scope.namespaces.insert("n", nmspc_scope);

            scope
        })
    }

    #[test]
    fn test_expr2ir_unop_ident() {
        // unop ident
        assert_eq!(
            expr2ir(
                &Expr::Unop(Unop::Neg, Box::new(
                        Expr::Ident(Ident {name: "w", namespace: vec!["n"]}))),
                get_test_scope(), IRType::F32),
            vec![
                (IRType::F32, IR::LitFloat(3.7)),
                (IRType::F32, IR::Neg)
            ]
        );

        // nonexistent ident
        assert_eq!(
            expr2ir(
                &Expr::Unop(Unop::Neg, Box::new(
                        Expr::Ident(Ident {name: "nonexistetn", namespace: vec!["n"]}))),
                get_test_scope(), IRType::F32),
            vec![
                (IRType::Error, IR::Error),
                (IRType::Error, IR::Neg)
            ]
        )
    }

    #[test]
    fn test_expr2ir_binop_cast() {
        assert_eq!(
            expr2ir(
                &Expr::Binop(Binop::Add,
                    Box::new(Expr::Ident(Ident {name: "x", namespace: vec![]})),
                    Box::new(Expr::Binop(Binop::Mul,
                        Box::new(Expr::Lit(Literal::Int(5))),
                        Box::new(Expr::Lit(Literal::Int(9)))))),
                get_test_scope(), IRType::I64),
            vec![
                (IRType::I32, IR::LocalGet("raw_x".to_string())),
                (IRType::I64, IR::Cast(IRType::I32)),
                (IRType::I64, IR::LitInt(5)),
                (IRType::I64, IR::LitInt(9)),
                (IRType::I64, IR::Mul(false)),
                (IRType::I64, IR::Add),
            ]
        )
    }

    #[test]
    fn test_expr2ir_proc_call() {
        assert_eq!(
            expr2ir(
                &Expr::ProcCall(Ident {name: "f", namespace: vec![]}, vec![
                    Expr::Lit(Literal::Int(5)),
                    Expr::Lit(Literal::Float(3.7)),
                ]),
                get_test_scope(), IRType::F32),
            vec![
                (IRType::I32, IR::LitInt(5)),
                (IRType::F64, IR::LitFloat(3.7)),
                (IRType::F64, IR::Call("raw_f".to_string())),
                (IRType::F32, IR::Cast(IRType::F64))
            ]
        )
    }
}
