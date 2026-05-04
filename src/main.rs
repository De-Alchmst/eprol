pub mod lexer;
pub mod ast;
pub mod parser;
pub mod ir;
pub mod semantic;
pub mod semantic_types;
pub mod semantic_expressions;
pub mod semantic_top_level;
pub mod semantic_statements;
pub mod codegen;
pub mod name_encoding;
pub mod errors;

use std::env;
use semantic::analyse_and_compile;


fn main() {
    let srgs = env::args().collect::<Vec<String>>();
    if srgs.len() != 2 {
        eprintln!("Usage: {} <source-file>", srgs[0]);
        std::process::exit(1);
    }

    analyse_and_compile(&srgs[1]);
}
