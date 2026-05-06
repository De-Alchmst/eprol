use crate::{
    ast::*,
    ir::*,
    errors::{
        report_semantic_error,
    },
    name_encoding::{
        raw_name,
        raw_arg_name,
    },
    semantic_types::{
        Scope,
        ScopeItem,
        LitVal,
    },
    semantic_expressions::{
        expr2ir,
        irlist2lit,
    },
    semantic_statements::{
        stmt2ir,
    },
};
use std::{
    thread,
    collections::HashSet,
};
use chumsky::span::SimpleSpan;


pub fn process_imports<'a>(
    imports: Vec<TopLevel<'a>>,
    scope: &mut Scope,
    source_name: &String,
    source: &String,
) -> ImportIRList<'a> {
    let mut import_ir: ImportIRList = vec![];

    for top_level in imports {
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
                    &span, source_name, source,
                    "Identifier redeclaration",
                    format!("Identifier `{}` is already declared in scope",
                            inner.name)
                );
            }
        }
    }

    return import_ir;
}


pub fn process_const_decls(
    decls: ConstDeclBlock,
    nmspc: Vec<&str>,
    scope: &mut Scope,
    source_name: &String,
    source: &String,
) {
    for (span, name, expr) in decls {
        process_single_const_decl(scope, &nmspc, span, name, expr,
                                  source_name, &source);
    }
}


pub fn process_var_decls(
    vars: Vec<TopLevel>,
    scope: &mut Scope,
    source_name: &String,
    source: &String,
) -> TopLevelIRList {
    let mut var_ir: TopLevelIRList = vec![];

    for top_level in vars {
        if let TopLevel::VarDecl(nmspc, decls) = top_level {
            process_single_var_decls(scope, &nmspc, decls, &mut var_ir,
                                     source_name, source);
        }
    }

    return var_ir;
}


pub fn process_proc_decls(
    procs: Vec<TopLevel>,
    scope: &mut Scope,
    source_name: &String,
    source: &String,
) -> TopLevelIRList {
    let mut proc_ir: TopLevelIRList = vec![];

    // procs need to be split into two passes
    // first puts proc into scope and second builds the IR
    // this is to allow for recursive and mutually recursive procs
    register_procs(&procs, scope, source_name, source);

    thread::scope(|s| {
        let mut handles = vec![];

        for top_level in procs {
            handles.push(s.spawn(||
                process_proc_decl_body(top_level, &*scope, source_name, source)))
        }

        for handle in handles {
            proc_ir.push(handle.join().unwrap());
        }
    });

    return proc_ir;
}


pub fn process_accessors(
    accessors: Vec<TopLevel>,
    scope: &mut Scope,
    source_name: &String,
    source: &String,
) {
    for top_level in accessors {
        if let TopLevel::AccessorDecl(span, idnt, accessor) = top_level {
            // if already exists
            if !matches!(scope.search(&idnt), ScopeItem::None) {
                report_semantic_error(
                    &span, source_name, source,
                    "Identifier redeclaration",
                    format!("Identifier `{}` is already declared in scope",
                            idnt.name)
                );
                continue;
            }

            let expr_ir = expr2ir(&accessor.offset, &scope, IRType::I32,
                                  source_name, source);

            // if expression is literal
            if let Some(LitVal::Int(val)) = irlist2lit(&expr_ir) {
                scope.insert(&idnt, ScopeItem::Accessor(accessor.typ,
                                                        val * accessor.offset_len))
            } else {
                report_semantic_error(
                    &span, source_name, &source,
                    "Non-literal initializer",
                    "initializer must be comptime-known value".to_string()
                );
            }

        }
    }
}


////////////////////////////////////////////////////////////////////////////////


// registers proc headers in scope for forward-declaration and recursion reasons
fn register_procs(
    procs: &Vec<TopLevel>,
    scope: &mut Scope,
    source_name: &String,
    source: &String,
) {
    for top_level in procs {
        if let TopLevel::ProcDecl(span_idnt, args, ret_type, _export, _decls, _body)
               = top_level
        {
            let (span, idnt) = span_idnt;
            let raw_name = raw_name(&idnt, source_name);
            let ret_type = asttype2irtype(ret_type.clone());

            // if already exists
            if matches!(scope.search(&idnt), ScopeItem::Proc(_, _, _)) {
                report_semantic_error(
                    &span, source_name, &source,
                    "Identifier redeclaration",
                    format!("Identifier `{}` is already declared in scope", idnt.name)
                );
                continue;
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
}


fn process_proc_decl_body(
    top_level: TopLevel,
    scope: &Scope,
    source_name: &String,
    source: &String,
) -> TopLevelIR {
    // rust still thinks it can be whatever
    let (span_idnt, args, ret_type, export, decls, body) =
        match top_level {
            TopLevel::ProcDecl(span_idnt, args, ret_type, export, decls, body)
              => (span_idnt, args, ret_type, export, decls, body),
            _ => unreachable!()
        };

    let (_, idnt) = span_idnt;
    let raw_name = raw_name(&idnt, source_name);
    let ret_type = asttype2irtype(ret_type);

    // will fork scope for local proc
    // will allow shadowing, but `local_set` used to prevent duplicate
    // idents within the function scope first one does n
    let mut local_scope = scope.clone();
    let mut local_set: HashSet<String> = HashSet::new();
    let mut proc_args: Vec<(String, IRType)> = vec![];

    // register proc args
    for (typ, names) in args {
        let typ = asttype2irtype(typ);
        for (span, name) in names {
            let raw_arg_name = raw_arg_name(name, &raw_name);

            // if doesn't exist yet
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

    // TODO: can probably be done better
    for decl in decls {
        match decl {
            ProcDeclBlock::Const(cdecls) => {
                process_const_decls(cdecls.clone(), vec![], &mut local_scope,
                                    source_name, source);
                for (_span, name, _expr) in cdecls {
                    local_set.insert(raw_arg_name(name, &raw_name));
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
    
    TopLevelIR::Proc(raw_name, proc_args, ret_type, 
                     if matches!(export, Some(_)) {
                         export.map(|s| s.to_string())
                     } else { None },
                     local_vars, body_ir)
}


/// Process a const declaration block
/// Evaluates constant expressions and adds them to the scope
fn process_single_const_decl(
    scope: &mut Scope,
    nmspc: &[&str],
    span: SimpleSpan,
    name: &str,
    expr: Expr,
    source_name: &String,
    source: &str,
) {
    let idnt = Ident {name, namespace: nmspc.to_vec()};

    // if already exists exist
    if !matches!(scope.search(&idnt), ScopeItem::None) {
        report_semantic_error(
            &span, source_name, &source,
            "Identifier redeclaration",
            format!("Identifier `{}` is already declared in scope", name)
        );
        return;
    }

    let expr_ir = expr2ir(&expr, &scope, IRType::Any, source_name, source);
    // if expression is literal
    if let Some(lit_val) = irlist2lit(&expr_ir) {
        scope.insert(&idnt, ScopeItem::Const(lit_val));
    } else {
        report_semantic_error(
            &span, source_name, &source,
            "Non-literal initializer",
            "initializer must be comptime-known value".to_string()
        );
    }
}


// Process a variable declaration block
// Declares variables in scope and generates global variable IR
fn process_single_var_decls(
    scope: &mut Scope,
    nmspc: &[&str],
    decls: Vec<VarDeclBlock>,
    regular_ir: &mut TopLevelIRList,
    source_name: &String,
    source: &str,
) {
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

            // Pre-compute initialization IR
            let init_ir = match init_expr {
                None => default_irtype_val(&typ),
                Some(expr) => {
                    let expr_ir = expr2ir(&expr, &scope, typ.clone(),
                                          source_name, &source);

                    // if it is not comptime-known
                    if !matches!(irlist2lit(&expr_ir), Some(_)) {
                        report_semantic_error(
                            &expr2span(&expr),
                            source_name, &source,
                            "Non-literal initializer",
                            "initializer must be comptime-known value".to_string()
                        );
                        continue;
                    }

                    expr_ir.last().unwrap().clone()
                }
            };

            scope.insert(&idnt,
                         ScopeItem::Var(raw_name.clone(), typ.clone(), false));
            regular_ir.push(TopLevelIR::GlobalVar(raw_name, init_ir));
        }
    }
}
