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
    },
    semantic_expressions::{
        expr2ir,
    },
    name_encoding::{
        raw_name,
    }
};
use chumsky::span::SimpleSpan;

pub fn process_imports<'a, 'b : 'a>(
    imports: Vec<TopLevel<'b>>,
    scope: &'a mut Scope,
    source_name: &'b String,
    source: &'b String,
) -> ImportIRList<'b> {
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
                    format!("Identifier `{}` is already declared in scope", inner.name)
                );
            }
        }
    }

    return import_ir;
}


pub fn process_const_decls<'a, 'b : 'a>(
    decls: ConstDeclBlock<'b>,
    nmspc: Vec<&'b str>,
    scope: &'a mut Scope,
    source_name: &'b String,
    source: &'b String,
) {
    for (span, name, expr) in decls {
        process_single_const_decl(scope, &nmspc, span, name, expr,
                                  source_name, &source);
    }
}


pub fn process_var_decls<'a, 'b : 'a>(
    vars: Vec<TopLevel<'b>>,
    scope: &'a mut Scope,
    regular_ir: &'a mut TopLevelIRList,
    source_name: &'b String,
    source: &'b String,
) {
    for top_level in vars {
        if let TopLevel::VarDecl(nmspc, decls) = top_level {
            process_single_var_decls(scope, &nmspc, decls, regular_ir,
                                     source_name, source);
        }
    }
}


////////////////////////////////////////////////////////////////////////////////


/// Process a const declaration block
/// Evaluates constant expressions and adds them to the scope
fn process_single_const_decl<'a>(
    scope: &mut Scope,
    nmspc: &[&'a str],
    span: SimpleSpan,
    name: &'a str,
    expr: Expr<'a>,
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
fn process_single_var_decls<'a>(
    scope: &mut Scope,
    nmspc: &[&'a str],
    decls: Vec<VarDeclBlock<'a>>,
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


// extract literal from IRList if possible
fn irlist2lit(ir: &IRList) -> Option<LitVal> {
    match ir.last() {
        Some((_, IR::LitInt(x)))   => Some(LitVal::Int(*x)),
        Some((_, IR::LitFloat(x))) => Some(LitVal::Float(*x)),
        Some((_, IR::LitStr(s)))   => Some(LitVal::Str(s.clone())),
        _ => None
    }
}

