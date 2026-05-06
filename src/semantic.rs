use crate::{
    ast::*,
    parser::parse_str_program,
    errors::{
        error_appeared,
    },
    codegen::*,
    semantic_top_level::{
        process_imports,
        process_const_decls,
        process_var_decls,
        process_proc_decls,
    },
    semantic_types::{
        Scope,
        get_data_set,
    }
};
use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string
};


pub fn analyse_and_compile<'a>(source_name: &String) -> HashSet<&'a str> {
    let source = read_to_string(source_name).expect("Failed to read source file");
    let ast = parse_str_program(&source, source_name);

    let output_files = HashSet::new();
    let mut scope = Scope {
        contents: HashMap::new(),
        namespaces: HashMap::new(),
    };

    if ast.len() == 0 {
        return output_files;
    }

    // needs to process statements in specific order
    let mut imports_to_process:   Vec<TopLevel> = vec![];
    let mut vars_to_process:      Vec<TopLevel> = vec![];
    let mut procs_to_process:     Vec<TopLevel> = vec![];
    let mut accessors_to_process: Vec<TopLevel> = vec![];

    // CONST processing
    // and fill other lists for later processing
    for top_level in ast {
        match top_level {
            TopLevel::Import(_, _, _, _) => imports_to_process.push(top_level),
            TopLevel::VarDecl(_, _) => vars_to_process.push(top_level),
            TopLevel::ProcDecl(_, _, _, _, _, _) => procs_to_process.push(top_level),
            TopLevel::AccessorDecl(_, _, _) => accessors_to_process.push(top_level),
            TopLevel::ConstDecl(nmspc, decls) => {
                process_const_decls(decls, nmspc, &mut scope, source_name, &source);
            }
        }
    }

    // ACCESSORS

    // IMPORTS
    let import_ir = process_imports(imports_to_process,
                                    &mut scope, source_name, &source);
    // VARS
    let var_ir = process_var_decls(vars_to_process, &mut scope,
                                   source_name, &source);
    // PROCS
    let proc_ir = process_proc_decls(procs_to_process, &mut scope,
                                     source_name, &source);

    // don't produce any code if errors found
    if !error_appeared() {
        print_wat_header();
        print_import_ir(&import_ir);
        print_memory(1, vec!["env", "memory"]);
        print_data(0x19a0, get_data_set().lock().unwrap().iter().cloned().collect());
        print_top_level_ir(&var_ir);
        print_top_level_ir(&proc_ir);
        print_wat_footer();
    }
    
    output_files
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        sync::{OnceLock},
    };
    use crate::{
        ir::*,
        semantic_types::{
            LitVal,
            ScopeItem,
        },
        semantic_expressions::{
            expr2ir,
        }
    };

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
                &Expr::Unop(PS, Unop::Not, Box::new(
                        Expr::Ident(PS, Ident {name: "nonexistent", namespace: vec!["n"]}))),
                get_test_scope(), IRType::F32, &String::new(), ""),
            vec![
                (IRType::Error, IR::Error),
                (IRType::Error, IR::Not)
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
