use crate::{
    ast::*,
    ir::*,
    parser::parse_str_program,
    name_encoding::{
        raw_name,
        raw_arg_name,
    },
    codegen::*,
};
use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
    fs::read_to_string
};

// types used for Scope representation
#[derive(Clone)]
enum LitVal { Int(i64), Float(f64), Str(String) }
#[derive(Clone)]
enum ScopeItem {
    Var(String, IRType), // raw_name, type
    Const(LitVal), // value
    Proc(String, Vec<IRType>, IRType), // raw_name, arg types, return type
    None, // used as a return value
}

#[derive(Clone)]
struct Scope {
    contents: HashMap<String, ScopeItem>,
    namespaces: HashMap<String, Scope>,
}

// cache for already processed files imported at multiple places
static FILE_CACHE: OnceLock<HashMap<&str, Scope>> = OnceLock::new();
fn get_file_cache<'a>() -> &'static HashMap<&'a str, Scope> {
    FILE_CACHE.get_or_init(|| HashMap::new())
}

// flag to signal whether compilation finished successfully or not
// don't join files if errors found
pub static mut FOUND_ERRORS: bool = false;
pub fn analyse_and_compile<'a>(source_name: &String) -> HashSet<&'a str> {
    let source = read_to_string(source_name).expect("Failed to read source file");
    let ast = parse_str_program(&source).expect("Failed to parse source file");

    let output_files = HashSet::new();
    let mut scope = Scope {
        contents: HashMap::new(),
        namespaces: HashMap::new(),
    };

    // imports must be first
    let mut import_ir: ImportIRList = vec![];
    let mut regular_ir: TopLevelIRList = vec![];

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
                process_const_decl(&mut scope, &nmspc, &decls);
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
                        scope.insert(&inner, ScopeItem::Proc(raw_name.clone(),
                                     arg_types, *ret_type));
                    },

                    IRType::I32 | IRType::I64 | IRType::F32 | IRType::F64 => {
                        scope.insert(&inner, ScopeItem::Var(raw_name.clone(),
                                     typ.clone()));
                    },
                    
                    // void
                    _ => unreachable!()
                }

                import_ir.push((outer, raw_name, typ));
            } else {
                // TODO: handle error of redeclaration
            }
        }
    }


    // VARS
    for top_level in vars_to_process {
        if let TopLevel::VarDecl(nmspc, decls) = top_level {
            process_var_decl(&mut scope, &nmspc, decls, &mut regular_ir, source_name);
        }
    }


    // PROCS
    for top_level in procs_to_process {
        if let TopLevel::ProcDecl(idnt, args, ret_type, export, _decl, _body)
               = top_level
        {
            let raw_name = raw_name(&idnt, source_name);
            let ret_type = asttype_to_irtype(ret_type);

            if matches!(scope.search(&idnt).0, ScopeItem::Proc(_, _, _)) {
                // TODO: handle error of redeclaration
            }

            // will fork scope for local proc
            // will allow shadowing, but `local_set` used to prevent duplicate
            // idents within the function scope
            // will_ not add to scope right aways, since it needs to add self first
            let mut local_set: HashSet<String> = HashSet::new();
            let mut proc_args: Vec<(&str, String, IRType)> = vec![];

            // register proc args
            for (typ, names) in args {
                let typ = asttype_to_irtype(typ);
                for name in names {
                    let raw_arg_name = raw_arg_name(name, &raw_name);
                    if !local_set.contains(&raw_arg_name.clone()) {
                        proc_args.push((name, raw_arg_name.clone(), typ.clone()));
                        local_set.insert(raw_arg_name);
                    } else {
                        // TODO: handle error of duplicate argument name
                    }
                }
            }

            // insert self to global scope and fork it as local
            scope.insert(&idnt, ScopeItem::Proc(raw_name.clone(),
                         proc_args.iter().map(|(_, _, typ)| typ.clone()).collect(),
                         ret_type.clone()));
            let mut local_scope = scope.clone();

            // add args to local scope and format args for IR
            let mut final_args: Vec<(String, IRType)> = vec![];
            for (name, raw_arg_name, typ) in proc_args.iter() {
                local_scope.insert(&Ident {name: name, namespace: vec![]},
                                   ScopeItem::Var(raw_arg_name.clone(), typ.clone()));
                final_args.push((raw_arg_name.clone(), typ.clone()));
            }

            // TODO: add locals
            let local_vars: Vec<(String, IRType)> = vec![];
            // TODO: add body
            let body_ir: IRList = vec![];
            
            regular_ir.push(TopLevelIR::Proc(raw_name, final_args, ret_type, 
                                             if let Some(s) = export {
                                                 Some(s.to_string())
                                             } else { None },
                                             local_vars, body_ir));
        }
    }

    print_wat_header();
    print_import_ir(&import_ir);
    print_top_level_ir(&regular_ir);
    print_wat_footer();


    
    output_files
}


/// Process a const declaration block
/// Evaluates constant expressions and adds them to the scope
fn process_const_decl<'a>(
    scope: &mut Scope,
    nmspc: &[&'a str],
    decls: &ConstDeclBlock<'a>
) {
    // move from here
    for (name, expr) in decls {
        let idnt = Ident {name, namespace: nmspc.to_vec()};
        let (val, _localp) = scope.search(&idnt);

        if let ScopeItem::None = val {
            let expr_ir = expr2ir(&expr, &scope, IRType::Any);
            // if expression is literal
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


/// Process a variable declaration block
/// Declares variables in scope and generates global variable IR
fn process_var_decl<'a>(
    scope: &mut Scope,
    nmspc: &[&'a str],
    decls: Vec<VarDeclBlock<'a>>,
    regular_ir: &mut TopLevelIRList,
    source_name: &str
) {
    // move from here
    for (typ, vals) in decls {
        let typ = asttype_to_irtype(typ);

        for (name, init_expr) in vals {
            let idnt = Ident {name, namespace: nmspc.to_vec()};
            let raw_name = raw_name(&idnt, source_name);

            // Already in scope
            if !matches!(scope.search(&idnt).0, ScopeItem::None) {
                // TODO: handle error of redeclaration
                continue;
            }

            // Pre-compute initialization IR (immutable borrow of scope)
            let init_ir = match init_expr {
                None => default_irtype_val(&typ),
                Some(expr) => {
                    let expr_ir = expr2ir(&expr, &scope, typ.clone());
                    if let Some(_) = irlist_lit(&expr_ir) {
                        expr_ir.last().unwrap().clone()
                    } else {
                        // TODO: handle non-literal var initializer
                        continue;
                    }
                }
            };

            scope.insert(&idnt, ScopeItem::Var(raw_name.clone(),
                                               typ.clone()));
            regular_ir.push(TopLevelIR::GlobalVar(raw_name, init_ir));
        }
    }
}




impl Scope {
    fn search(&self, ident: &Ident) -> (ScopeItem, bool) {
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


    fn insert(&mut self, ident: &Ident, item: ScopeItem) -> bool {
        let mut current_scope = self;

        // step through namespaces to find required scope, create if not found
        for i in 0..ident.namespace.len() {
            if !current_scope.namespaces.contains_key(ident.namespace[i]) {
                current_scope.namespaces.insert(ident.namespace[i].to_string(), Scope {
                    contents: HashMap::new(),
                    namespaces: HashMap::new(),
                });
            }
            current_scope = current_scope.namespaces.get_mut(ident.namespace[i])
                                                    .unwrap();
        }

        if current_scope.contents.contains_key(ident.name) {
            return false;
        } else {
            current_scope.contents.insert(ident.name.to_string(), item);
            true
        }
    }
}


fn irlist_lit(ir: &IRList) -> Option<LitVal> {
    match ir.last() {
        Some((_, IR::LitInt(x))) => Some(LitVal::Int(*x)),
        Some((_, IR::LitFloat(x))) => Some(LitVal::Float(*x)),
        Some((_, IR::LitStr(s))) => Some(LitVal::Str(s.clone())),
        _ => None
    }
}


// TODO: handle errors somehow
fn expr2ir<'a>(expr: &Expr<'a>, scope: &Scope, expects: IRType) -> IRList {
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
                IRType::Int | IRType::I32 => vec![(IRType::I32, IR::LitStr(s.to_string()))],
                IRType::I64 => vec![(IRType::I64, IR::LitStr(s.to_string()))],
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
            match &val {
                // constants -> place as literals
                ScopeItem::Const(lit_val) => match lit_val {
                    LitVal::Int(x)
                        => expr2ir(&Expr::Lit(Literal::Int(*x)), scope, expects),
                    LitVal::Float(x)
                        => expr2ir(&Expr::Lit(Literal::Float(*x)), scope, expects),
                    // String constants not supported yet - would need owned strings in IR
                    LitVal::Str(_) => vec![(IRType::Error, IR::Error)],
                },

                // variables -> place and cast if needed
                ScopeItem::Var(raw_name, var_type) => {
                    let expr = if localp {
                        (var_type.clone(), IR::LocalGet(raw_name.clone()))
                    } else {
                        (var_type.clone(), IR::GlobalGet(raw_name.clone()))
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
    fn get_test_scope<'a>() -> &'static Scope {
        TEST_SCOPE.get_or_init(|| {
            let mut scope = Scope {
                contents: HashMap::new(),
                namespaces: HashMap::new(),
            };
            scope.contents.insert(String::from("x"),
                ScopeItem::Var("raw_x".to_string(), IRType::I32));
            scope.contents.insert(String::from("y"),
                ScopeItem::Const(LitVal::Int(42)));
            scope.contents.insert(String::from("f"),
                ScopeItem::Proc("raw_f".to_string(),
                                vec![IRType::I32, IRType::F64], IRType::F64));

            let mut nmspc_scope = Scope {
                contents: HashMap::new(),
                namespaces: HashMap::new(),
            };
            nmspc_scope.contents.insert(String::from("z"),
                ScopeItem::Var("raw_z".to_string(), IRType::F64));
            nmspc_scope.contents.insert(String::from("w"),
                ScopeItem::Const(LitVal::Float(3.7)));
            scope.namespaces.insert(String::from("n"), nmspc_scope);

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
