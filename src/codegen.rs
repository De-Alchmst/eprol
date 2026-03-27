use crate::{
    ir::*,
};

pub fn print_wat_header() {
    println!("(module")
}

pub fn print_wat_footer() {
    println!(")")
}


pub fn print_import_ir(lst: &ImportIRList) {
    for ir in lst {
        let (outer_name, raw_name, typ) = ir;
        {
            match typ {
                IRType::Func(arg_types, ret_type) => {
                    println!(" (import {} (func {} {} {}))",
                        outer_name.iter().map(|s| format!("\"{}\"", s))
                                  .collect::<Vec<String>>().join(" "),
                        raw_name,
                        arg_types.iter().map(|t| format!("(param {})", t))
                                  .collect::<Vec<String>>().join(" "),
                        irtype_to_return(&*ret_type))
                },

                _ => {
                    println!(" (global {} (import {}) (mut {}))",
                        raw_name,
                        outer_name.iter().map(|s| format!("\"{}\"", s))
                                  .collect::<Vec<String>>().join(" "),
                        typ)
                }
            }
        }
    }
}


pub fn print_top_level_ir(lst: &TopLevelIRList) {
}


pub fn print_wat_ir<'a>(lst: &'a IRList<'a>) {
    for (_typ, ir) in lst {
        match ir {
            _ => {}
        }
    }
}


fn irtype_to_return(typ: &IRType) -> String {
    match typ {
        IRType::Void => String::from(""),
        _ => format!("(result {})", typ)
    }
}
