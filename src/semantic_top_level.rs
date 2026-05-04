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
    name_encoding::{
        raw_name,
    }
};

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
