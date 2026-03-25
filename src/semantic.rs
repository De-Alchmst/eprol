/*
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
enum ConstVal { Int(i64), Float(f64), }
type Var<'a> = (&'a str, IRBaseType);
type Const<'a> = (&'a str, IRBaseType, ConstVal);
type Proc<'a> = (&'a str, Vec<IRBaseType>, IRBaseType);

struct Scope<'a> {
    vars: Vec<Var<'a>>,
    consts: Vec<Const<'a>>,
    procs: Vec<Proc<'a>>,
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

    let mut output_files = HashSet::new();
    let scope = Scope {
        vars: vec![],
        consts: vec![],
        procs: vec![],
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
            TopLevel::ConstDecl(nmspc, decls) => {
                for (ident, expr) in decls {
                }
            }
        }
    }
    
    output_files
}


fn expr2ir<'a>(expr: Expr<'a>, expects: IRExtType) -> IR<'a> {
    match expr {
        Expr::Lit(lit) => match lit {
            Literal::Int(x) => IR::LitInt(l),
            Literal::Float(x) => IR::LitFloat(x),
            Literal::Str(s) => IR::LitStr(s),
        }
        _ => unimplemented!(),
    }
}
*/
