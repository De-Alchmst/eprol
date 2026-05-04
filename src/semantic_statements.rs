use crate::{
    ast::*,
    ir::*,
    errors::{
        report_semantic_error,
    },
    semantic_types::{
        Scope,
    },
    semantic_expressions::{
        expr2ir,
        left_value2ir,
    },
};


pub fn stmt2ir(
    stmt: &Stmt, scope: &Scope,
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
