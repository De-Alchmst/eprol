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
    for ir in lst {
        match ir {
            TopLevelIR::GlobalVar(raw_name, val) => {
                let (typ, _) = val;
                println!(" (global {} (mut {}) {})",
                         raw_name, typ, ir_to_str(val))
            }
        }
    }
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

fn ir_to_str(ir: &(IRType, IR)) -> String {
    let (typ, val) = ir;
    match val {
        IR::LitInt(i) => format!("{}.const {}", typ, i),
        IR::LitFloat(f) => format!("{}.const {}", typ, f),
        IR::Drop => format!("drop"),
        IR::Call(raw_name) => format!("call {}", raw_name),
        _ => unimplemented!()
    }
}
