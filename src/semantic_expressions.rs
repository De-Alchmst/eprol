use crate::{
    ast::*,
    ir::*,
    errors::{
        report_semantic_error,
    },
    semantic_types::{
        Scope,
        ScopeItem,
        LitVal,
        get_data_set,
    },
};


pub fn expr2ir<'a>(
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

        // ACCESSORS
        Expr::Access(_span, _exp, _access) => {
            vec![(expects, IR::Error)]
        }

        // Malformed expressions, errors are already reported by the parser
        Expr::Malformed => vec![(expects, IR::Error)]
    }
}

pub fn left_value2ir<'a>(
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

