use crate::{
    ir::*,
};

pub fn print_wat_header() {
    println!("(module")
}

pub fn print_wat_footer() {
    println!(")")
}

pub fn print_wat_ir<'a>(lst: &'a IRList<'a>) {
    for (_typ, ir) in lst {
        match ir {

            _ => {}
        }
    }
}
