use crate::{
    ast::*,
    ir::*,
    parser::parse_str_program,
    errors::{
        report_semantic_error,
        error_appeared,
    },
    name_encoding::{
        raw_name,
        raw_arg_name,
    },
    codegen::*,
};
use std::{
    collections::{HashMap, HashSet},
    sync::{Mutex, OnceLock},
    fs::read_to_string
};
use chumsky::span::SimpleSpan;

// types used for Scope representation
#[derive(Clone, Debug)]
enum LitVal { Int(i64), Float(f64), Str(String) }
#[derive(Clone, Debug)]
enum ScopeItem {
    Var(String, IRType, bool), // raw_name, type, localp
    Const(LitVal), // value
    Proc(String, Vec<IRType>, IRType), // raw_name, arg types, return type
    None, // used as a return value
}

#[derive(Clone)]
struct Scope {
    contents: HashMap<String, ScopeItem>,
    namespaces: HashMap<String, Scope>,
}

// GLOBAL STATE //

// cache for already processed files imported at multiple places
static _FILE_CACHE: OnceLock<Mutex<HashMap<&str, Scope>>> = OnceLock::new();
fn _get_file_cache<'a>() -> &'static Mutex<HashMap<&'a str, Scope>> {
    _FILE_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

// used to store data for the wasm data section
// currently only strings
static DATA_SET: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
fn get_data_set<'a>() -> &'static Mutex<HashSet<String>> {
    DATA_SET.get_or_init(|| Mutex::new(HashSet::new()))
}



pub fn analyse_and_compile<'a>(source_name: &String) -> HashSet<&'a str> {
    let source = read_to_string(source_name).expect("Failed to read source file");
    let ast = parse_str_program(&source, source_name, &source);

    let output_files = HashSet::new();
    let mut scope = Scope {
        contents: HashMap::new(),
        namespaces: HashMap::new(),
    };

    if ast.len() == 0 {
        return output_files;
    }

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
            TopLevel::Import(_, _, _, _) => imports_to_process.push(top_level),
            TopLevel::VarDecl(_, _) => vars_to_process.push(top_level),
            TopLevel::ProcDecl(_, _, _, _, _, _) => procs_to_process.push(top_level),
            TopLevel::ConstDecl(nmspc, decls) => {
                for (span, name, expr) in decls {
                    process_const_decl(&mut scope, &nmspc, span, name, expr,
                                       source_name, &source);
                }
            }
        }
    }


    // IMPORTS
    for top_level in imports_to_process {
        if let TopLevel::Import(span, outer, inner, typ) = top_level {
            if matches!(scope.search(&inner), ScopeItem::None) {
                let typ = asttype2irtype(typ);
                let raw_name = raw_name(&inner, source_name);
                
                match typ.clone() {
                    IRType::Func(arg_types, ret_type) => {
                        scope.insert(&inner, ScopeItem::Proc(raw_name.clone(),
                                     arg_types, *ret_type));
                    },

                    IRType::I32 | IRType::I64 | IRType::F32 | IRType::F64 => {
                        scope.insert(&inner, ScopeItem::Var(raw_name.clone(),
                                     typ.clone(), false));
                    },
                    
                    // void
                    _ => unreachable!()
                }

                import_ir.push((outer, raw_name, typ));

            } else {
                report_semantic_error(
                    &span, source_name, &source,
                    "Identifier redeclaration",
                    format!("Identifier `{}` is already declared in scope", inner.name)
                );
            }
        }
    }


    // VARS
    for top_level in vars_to_process {
        if let TopLevel::VarDecl(nmspc, decls) = top_level {
            process_var_decls(&mut scope, &nmspc, decls, &mut regular_ir,
                              source_name, &source);
        }
    }


    // PROCS
    // procs need to be split into two passes
    // first puts proc into scope and second builds the IR
    // this is to allow for recursive and mutually recursive procs
    for top_level in &procs_to_process {
        if let TopLevel::ProcDecl(span_idnt, args, ret_type, _export, _decls, _body)
               = top_level
        {
            let (span, idnt) = span_idnt;
            let raw_name = raw_name(&idnt, source_name);
            let ret_type = asttype2irtype(ret_type.clone());

            if matches!(scope.search(&idnt), ScopeItem::Proc(_, _, _)) {
                report_semantic_error(
                    &span, source_name, &source,
                    "Identifier redeclaration",
                    format!("Identifier `{}` is already declared in scope", idnt.name)
                );
            }

            let mut proc_args: Vec<IRType> = vec![];
            for (typ, names) in args {
                for _ in names {
                    proc_args.push(asttype2irtype(typ.clone()));
                }
            }

            scope.insert(&idnt, ScopeItem::Proc(raw_name.clone(),
                         proc_args, ret_type.clone()));
        }
    }
    
    for top_level in procs_to_process {
        if let TopLevel::ProcDecl(span_idnt, args, ret_type, export, decls, body)
               = top_level
        {
            let (_span, idnt) = span_idnt;
            let raw_name = raw_name(&idnt, source_name);
            let ret_type = asttype2irtype(ret_type);

            // will fork scope for local proc
            // will allow shadowing, but `local_set` used to prevent duplicate
            // idents within the function scope
            // will not add to scope right aways, since it needs to add self first
            let mut local_scope = scope.clone();
            let mut local_set: HashSet<String> = HashSet::new();
            let mut proc_args: Vec<(String, IRType)> = vec![];

            // register proc args
            for (typ, names) in args {
                let typ = asttype2irtype(typ);
                for (span, name) in names {
                    let raw_arg_name = raw_arg_name(name, &raw_name);
                    if !local_set.contains(&raw_arg_name.clone()) {
                        local_set.insert(raw_arg_name.clone());
                        proc_args.push((raw_arg_name.clone(), typ.clone()));
                        local_scope.insert(&Ident {name: name, namespace: vec![]},
                                           ScopeItem::Var(raw_arg_name,
                                                          typ.clone(), true));
                    } else {
                        report_semantic_error(
                            &span, source_name, &source,
                            "Identifier redeclaration",
                            format!("Identifier `{}` is already declared in scope", name)
                        );
                    }
                }
            }

            // locals
            // adds variable initialisations to `body_ir`
            let mut local_vars: Vec<(String, IRType)> = vec![];
            let mut body_ir: IRList = vec![];

            for decl in decls {
                match decl {
                    ProcDeclBlock::Const(cdecls) => {
                        for (span, name, expr) in cdecls {
                            local_set.insert(raw_arg_name(name, &raw_name));
                            process_const_decl(&mut local_scope, &[], span, name,
                                               expr, source_name, &source);
                        }
                    }

                    // cannot use `process_var_decls`, since it does not allow
                    // shadowing and does not add anything to `local_vars`
                    // and few other things...
                    ProcDeclBlock::Var(vdecls) => {
                        for (typ, vals) in vdecls {
                            let typ = asttype2irtype(typ);

                            for (span, name, init_expr) in vals {
                                let idnt = Ident {name, namespace: vec![]};
                                let raw_name = raw_arg_name(name, &raw_name);

                                // already in local scope
                                if local_set.contains(&raw_name.clone()) {
                                    report_semantic_error(
                                        &span, source_name, &source,
                                        "Identifier redeclaration",
                                        format!("Identifier `{}` is already declared in scope", name)
                                    );
                                    continue;
                                }


                                // Pre-compute initialization IR
                                let init_ir = match init_expr {
                                    None => vec![default_irtype_val(&typ)],
                                    Some(expr)
                                        => expr2ir(&expr, &local_scope, typ.clone(),
                                                   source_name, &source),
                                };

                                local_scope.insert(&idnt,
                                                   ScopeItem::Var(raw_name.clone(),
                                                                  typ.clone(), true));
                                // add local declaration
                                local_vars.push((raw_name.clone(), typ.clone()));

                                // init local variable
                                body_ir.extend(init_ir);
                                body_ir.push((IRType::Void,
                                              IR::LocalSet(raw_name.clone())));
                            }
                        }
                    }
                }
            }
            
            // body
            // TODO: check if all paths return a value
            for stmt in body {
                body_ir.extend(stmt2ir(&stmt, &local_scope, source_name,
                                       &source, ret_type.clone()));
            }
            
            regular_ir.push(TopLevelIR::Proc(raw_name, proc_args, ret_type, 
                                             if let Some(s) = export {
                                                 Some(s.to_string())
                                             } else { None },
                                             local_vars, body_ir));
        }
    }

    // don't produce any code if errors found
    if !error_appeared() {
        print_wat_header();
        print_import_ir(&import_ir);
        print_memory(1, vec!["env", "memory"]);
        print_data(0x19a0, get_data_set().lock().unwrap().iter().cloned().collect());
        print_top_level_ir(&regular_ir);
        print_wat_footer();
    }
    
    output_files
}


/// Process a const declaration block
/// Evaluates constant expressions and adds them to the scope
fn process_const_decl<'a>(
    scope: &mut Scope,
    nmspc: &[&'a str],
    span: SimpleSpan,
    name: &'a str,
    expr: Expr<'a>,
    source_name: &String,
    source: &str,
) {
    let idnt = Ident {name, namespace: nmspc.to_vec()};

    if matches!(scope.search(&idnt), ScopeItem::None) {
        let expr_ir = expr2ir(&expr, &scope, IRType::Any, source_name, source);
        // if expression is literal
        if let Some(lit_val) = irlist_to_lit(&expr_ir) {
            scope.insert(&idnt, ScopeItem::Const(lit_val));
        } else {
            report_semantic_error(
                &span, source_name, &source,
                "Non-literal initializer",
                "initializer must be comptime-known value".to_string()
            );
        }

    } else {
        report_semantic_error(
            &span, source_name, &source,
            "Identifier redeclaration",
            format!("Identifier `{}` is already declared in scope", name)
        );
    }
}


// Process a variable declaration block
// Declares variables in scope and generates global variable IR
fn process_var_decls<'a>(
    scope: &mut Scope,
    nmspc: &[&'a str],
    decls: Vec<VarDeclBlock<'a>>,
    regular_ir: &mut TopLevelIRList,
    source_name: &String,
    source: &str,
) {
    // move from here
    for (typ, vals) in decls {
        let typ = asttype2irtype(typ);

        for (span, name, init_expr) in vals {
            let idnt = Ident {name, namespace: nmspc.to_vec()};
            let raw_name = raw_name(&idnt, source_name);

            // Already in scope
            if !matches!(scope.search(&idnt), ScopeItem::None) {
                report_semantic_error(
                    &span, source_name, &source,
                    "Identifier redeclaration",
                    format!("Identifier `{}` is already declared in scope", name)
                );
                continue;
            }

            // Pre-compute initialization IR (immutable borrow of scope)
            let init_ir = match init_expr {
                None => default_irtype_val(&typ),
                Some(expr) => {
                    let expr_ir = expr2ir(&expr, &scope, typ.clone(),
                                          source_name, &source);

                    if let Some(_) = irlist_to_lit(&expr_ir) {
                        expr_ir.last().unwrap().clone()
                    } else {
                        report_semantic_error(
                            &expr2span(&expr),
                            source_name, &source,
                            "Non-literal initializer",
                            "initializer must be comptime-known value".to_string()
                        );
                        continue;
                    }
                }
            };

            scope.insert(&idnt, ScopeItem::Var(raw_name.clone(),
                                               typ.clone(), false));
            regular_ir.push(TopLevelIR::GlobalVar(raw_name, init_ir));
        }
    }
}



impl Scope {
    fn search(&self, ident: &Ident) -> ScopeItem {
        let mut current_scope = self;

        // step through namespaces to find required scope, return Error if not found
        for i in 0..ident.namespace.len() {
            match current_scope.namespaces.get(ident.namespace[i]) {
                Some(ns) => {
                    current_scope = &ns;
                }
                None => return ScopeItem::None
            }
        }
        if let Some(var) = current_scope.contents.get(ident.name) {
            var.clone()
        } else {
            ScopeItem::None
        }
    }


    fn insert(&mut self, ident: &Ident, item: ScopeItem) {
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
        current_scope.contents.insert(ident.name.to_string(), item);
    }
}


fn irlist_to_lit(ir: &IRList) -> Option<LitVal> {
    match ir.last() {
        Some((_, IR::LitInt(x))) => Some(LitVal::Int(*x)),
        Some((_, IR::LitFloat(x))) => Some(LitVal::Float(*x)),
        Some((_, IR::LitStr(s))) => Some(LitVal::Str(s.clone())),
        _ => None
    }
}


fn stmt2ir<'a>(
    stmt: &Stmt<'a>, scope: &Scope,
    source_name: &String,
    source: &str,
    return_expects: IRType,
) -> IRList {
    match stmt {
        Stmt::Expr(expr) => {
            let mut ir = expr2ir(expr, scope, IRType::Any, source_name, source);
            if !ir.last().is_some_and(|(t, _)| matches!(t, IRType::Void)) {
                ir.push((IRType::Void, IR::Drop));
            };
            ir
        }

        Stmt::VoidReturn(span) => {
            if return_expects != IRType::Void {
                report_semantic_error(
                    span, source_name, source,
                    "Empty return",
                    format!("Return must be of type {:?}", return_expects)
                );
            };
            vec![(IRType::Void, IR::Return)]
        }

        Stmt::Return(span, expr) => {
            if return_expects == IRType::Void {
                report_semantic_error(
                    span, source_name, source,
                    "Unexpected return value",
                    "Return must be empty".to_string()
                );
                vec![(IRType::Void, IR::Return)]

            } else {
                let mut ir = expr2ir(expr, scope, return_expects.clone(),
                                     source_name, source);
                ir.push((IRType::Void, IR::Return));
                ir
            }
        }

        Stmt::Assign(left, right) => {
            let (left_ir, left_type) = left_value2ir(left, scope, source_name,
                                                     source);
            let mut right_ir = expr2ir(right, scope, left_type.clone(),
                                       source_name, source);
            right_ir.extend(left_ir);
            right_ir
        }

        _ => vec![(IRType::Void, IR::Error)] // TODO: handle other statements
    }
}


fn expr2ir<'a>(
    expr: &Expr<'a>, scope: &Scope, expects: IRType,
    source_name: &String,
    source: &str,
) -> IRList {
    match expr {
        // LITERALS
        Expr::Lit(span, lit) => match lit {
            Literal::Int(x) => match expects {
                IRType::Int | IRType::I64 | IRType::Any => vec![(IRType::I64, IR::LitInt(*x))],
                IRType::I32 => vec![(IRType::I32, IR::LitInt(*x))],
                IRType::Float | IRType::F64 => vec![(IRType::F64, IR::LitFloat(*x as f64))],
                IRType::F32 => vec![(IRType::F32, IR::LitFloat(*x as f64))],
                _ => {
                    report_semantic_error(
                        span, source_name, source,
                        "Incompatible types",
                        format!("Expected {:?}, found Int", expects)
                    );
                    vec![(IRType::Error, IR::Error)]
                }
            }

            Literal::Float(x) => match expects {
                IRType::Float | IRType::F64 | IRType::Any => vec![(IRType::F64, IR::LitFloat(*x))],
                IRType::F32 => vec![(IRType::F32, IR::LitFloat(*x))],
                _ => {
                    report_semantic_error(
                        span, source_name, source,
                        "Incompatible types",
                        format!("Expected {:?}, found Float", expects)
                    );
                    vec![(IRType::Error, IR::Error)]
                }
            }

            // TODO: handle strings like a normal person
            Literal::Str(s) => {
                let s = s.to_string();
                get_data_set().lock().unwrap().insert(s.clone());
                match expects {
                    IRType::Int | IRType::I32 | IRType::Any => vec![(IRType::I32, IR::LitStr(s.to_string()))],
                    IRType::I64 => vec![(IRType::I64, IR::LitStr(s))],
                    _ => {
                        report_semantic_error(
                            span, source_name, source,
                            "Incompatible types",
                            format!("Expected {:?}, found Int", expects)
                        );
                        vec![(IRType::Error, IR::Error)]
                    }
                }
            }
        }

        // UNOPS
        Expr::Unop(_span, op, inner) => {
            let mut inner_ir = expr2ir(inner, scope, expects, source_name, source);
            let inner_type   = irlist_type(&inner_ir);
            match op {
                Unop::Not => inner_ir.push((inner_type, IR::Not)),
                // TODO: do unop at compiletime with literals
                Unop::Neg => inner_ir.push((inner_type, IR::Neg)),
            }
            inner_ir
        }

        // IDENTS
        Expr::Ident(span, idnt) => {
            let val = scope.search(idnt);
            match &val {
                // constants -> place as literals
                ScopeItem::Const(lit_val) => match lit_val {
                    LitVal::Int(x)
                        => expr2ir(&Expr::Lit(PS, Literal::Int(*x)), scope,
                                   expects, source_name, source),
                    LitVal::Float(x)
                        => expr2ir(&Expr::Lit(PS, Literal::Float(*x)), scope,
                                   expects, source_name, source),
                    // String constants not supported yet - would need owned strings in IR
                    LitVal::Str(s)
                        => expr2ir(&Expr::Lit(PS, Literal::Str(s)), scope,
                                   expects, source_name, source),
                }

                // variables -> place and cast if needed
                ScopeItem::Var(raw_name, var_type, localp) => {
                    let expr = if *localp {
                        (var_type.clone(), IR::LocalGet(raw_name.clone()))
                    } else {
                        (var_type.clone(), IR::GlobalGet(raw_name.clone()))
                    };
                    ir_resolve_types(expr, expects, span, source_name, source)
                }

                // proc -> funcref not yet implemented -> error
                ScopeItem::Proc(_raw_name, _arg_types, _ret_type) => {
                    report_semantic_error(
                        span, source_name, source,
                        "Invalid identifier",
                        "Can not pass procedures as values yet".to_string()
                    );
                    vec![(IRType::Error, IR::Error)]
                }

                // not found -> error
                ScopeItem::None => {
                    report_semantic_error(
                        span, source_name, source,
                        "Unknown identifier",
                        format!("Identifier `{}` not found in scope", idnt.name)
                    );
                    vec![(IRType::Error, IR::Error)]
                }
            }
        }

        // BINOPS
        // left side determines result type
        // therefor right must match left and the entire outcome must then
        // match expects
        // TODO:: do binop at compiletime with literals
        // TODO:: add unsigned support for binops
        Expr::Binop(span, op, left, right) => {
            // evaluate both sides
            let mut left_ir = expr2ir(left, scope, IRType::Any,
                                      source_name, source);
            let mut left_type = irlist_type(&left_ir);
            if left_type == IRType::Error { left_type = IRType::Any; }

            let mut right_ir = expr2ir(right, scope, left_type.clone(),
                                       source_name, source);
            let right_type = irlist_type(&right_ir);

            // fix right type if needed
            if right_type != left_type {
                let last_right = right_ir.pop().unwrap_or((IRType::Error, IR::Error));
                right_ir.extend(ir_resolve_types(last_right, left_type.clone(),
                                                 span, source_name, source));
            }

            left_ir.extend(right_ir);
            left_ir.extend(
                ir_resolve_types((left_type, match op {
                    Binop::Add => IR::Add,
                    Binop::Sub => IR::Sub,
                    Binop::Mul => IR::Mul(false),
                    Binop::Div => IR::Div(false),
                    _ => {
                        report_semantic_error(
                            span, source_name, source,
                            "Unsupported operator",
                            format!("Operator {:?} not implemented yet", op)
                        );
                        IR::Error
                    }
                }), expects, span, source_name, source)
            );
            left_ir
        }

        // PROC CALLS
        Expr::ProcCall(span, idnt, args) => {
            let val = scope.search(idnt);
            match val {
                ScopeItem::Proc(raw_name, arg_types, ret_type) => {
                    // check args length
                    if arg_types.len() != args.len() {
                        report_semantic_error(
                            span, source_name, source,
                            "Invalid number of arguments",
                            format!("Expected {} arguments, found {}",
                                    arg_types.len(), args.len())
                        );
                        return vec![(IRType::Error, IR::Error)];
                    }

                    // evaluate args to their required types
                    let mut ir = vec![];
                    for i in 0..args.len() {
                        let arg_ir = expr2ir(&args[i], scope, arg_types[i].clone(),
                                             source_name, source);
                        ir.extend(arg_ir);
                    }
                    ir.extend(ir_resolve_types((ret_type.clone(), IR::Call(raw_name)),
                                               expects, span, source_name, source));
                    ir
                }

                ScopeItem::Const(_) | ScopeItem::Var(_, _, _) => {
                    report_semantic_error(
                        span, source_name, source,
                        "Not a procedure",
                        format!("Identifier `{}` is not a procedure", idnt.name)
                    );
                    vec![(IRType::Error, IR::Error)]
                }

                ScopeItem::None => {
                    report_semantic_error(
                        span, source_name, source,
                        "Unknown identifier",
                        format!("Identifier `{}` not found in scope", idnt.name)
                    );
                    vec![(IRType::Error, IR::Error)]
                }
            }
        }

        // Malformed expressions, errors are already reported by the parser
        Expr::Malformed => vec![(expects, IR::Error)],
    }
}

fn left_value2ir<'a>(
    lv: &LeftValue<'a>, scope: &Scope,
    source_name: &String,
    source: &str,
) -> (IRList, IRType) {
    match lv {
        LeftValue::Ident(span, idnt) => {
            let val = scope.search(idnt);
            match &val {
                // variables
                ScopeItem::Var(raw_name, var_type, localp) => {
                    (vec![(IRType::Void,
                            if *localp { IR::LocalSet (raw_name.clone()) }
                            else       { IR::GlobalSet(raw_name.clone()) })
                         ],
                     var_type.clone())
                }

                // constants
                ScopeItem::Const(_) => {
                    report_semantic_error(
                        span, source_name, source,
                        "Invalid left value",
                        "Cannot assign to constants".to_string()
                    );
                    (vec![(IRType::Error, IR::Error)], IRType::Any)
                }

                // proc
                ScopeItem::Proc(_raw_name, _arg_types, _ret_type) => {
                    report_semantic_error(
                        span, source_name, source,
                        "Invalid left value",
                        "Cannot assign to procedure".to_string()
                    );
                    (vec![(IRType::Error, IR::Error)], IRType::Any)
                }

                // not found -> error
                ScopeItem::None => {
                    report_semantic_error(
                        span, source_name, source,
                        "Unknown identifier",
                        format!("Identifier `{}` not found in scope", idnt.name)
                    );
                    (vec![(IRType::Error, IR::Error)], IRType::Any)
                }
            }
        }

        LeftValue::Malformed => {
            // malformed left value, error already reported by parser
            (vec![(IRType::Error, IR::Error)], IRType::Any)
        }
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
                ScopeItem::Var("raw_x".to_string(), IRType::I32, false));
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
                ScopeItem::Var("raw_z".to_string(), IRType::F64, false));
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
                &Expr::Unop(PS, Unop::Neg, Box::new(
                        Expr::Ident(PS, Ident {name: "w", namespace: vec!["n"]}))),
                get_test_scope(), IRType::F32, &String::new(), ""),
            vec![
                (IRType::F32, IR::LitFloat(3.7)),
                (IRType::F32, IR::Neg)
            ]
        );

        // nonexistent ident
        assert_eq!(
            expr2ir(
                &Expr::Unop(PS, Unop::Neg, Box::new(
                        Expr::Ident(PS, Ident {name: "nonexistent", namespace: vec!["n"]}))),
                get_test_scope(), IRType::F32, &String::new(), ""),
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
                &Expr::Binop(PS, Binop::Add,
                    Box::new(Expr::Ident(PS, Ident {name: "x", namespace: vec![]})),
                    Box::new(Expr::Binop(PS, Binop::Mul,
                        Box::new(Expr::Lit(PS, Literal::Int(5))),
                        Box::new(Expr::Lit(PS, Literal::Int(9)))))),
                get_test_scope(), IRType::I64, &String::new(), ""),
            vec![
                (IRType::I32, IR::GlobalGet("raw_x".to_string())),
                (IRType::I64, IR::LitInt(5)),
                (IRType::I64, IR::LitInt(9)),
                (IRType::I64, IR::Mul(false)),
                (IRType::I32, IR::Cast(IRType::I64)),
                (IRType::I32, IR::Add),
                (IRType::I64, IR::Cast(IRType::I32)), // ??
            ]
        )
    }

    #[test]
    fn test_expr2ir_proc_call() {
        assert_eq!(
            expr2ir(
                &Expr::ProcCall(PS, Ident {name: "f", namespace: vec![]}, vec![
                    Expr::Lit(PS, Literal::Int(5)),
                    Expr::Lit(PS, Literal::Float(3.7)),
                ]),
                get_test_scope(), IRType::F32, &String::new(), ""),
            vec![
                (IRType::I32, IR::LitInt(5)),
                (IRType::F64, IR::LitFloat(3.7)),
                (IRType::F64, IR::Call("raw_f".to_string())),
                (IRType::F32, IR::Cast(IRType::F64))
            ]
        )
    }
}
