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
                    println!(" (import {} (func {} (param {}) {}))",
                        outer_name.iter().map(|s| format!("\"{}\"", s))
                                  .collect::<Vec<String>>().join(" "),
                        raw_name,
                        arg_types.iter().map(|t| t.to_string())
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
            },

            TopLevelIR::Proc(raw_name, args, ret_type, export, locals, body) => {
                let export = if let Some(s) = export {
                    format!(" (export \"{}\")", s)
                 } else {
                     String::from("")
                 };
                 println!("\n (func {}{} {} {}\n   {}\n   {})",
                          raw_name, export,
                          args.iter().map(|(name, typ)|
                              format!("(param {} {})", name, typ))
                              .collect::<Vec<String>>().join(" "),
                          irtype_to_return(ret_type),
                          locals.iter()
                                .map(|(name, typ)| format!("(local {name} {typ})"))
                                .collect::<Vec<String>>().join("\n   "),
                          body.iter().map(|ir| ir_to_str(ir))
                              .collect::<Vec<String>>().join("\n   "))
            }
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
        IR::Drop => format!("drop"),
        IR::LitInt(i) => format!("{typ}.const {i}"),
        IR::LitFloat(f) => format!("{typ}.const {f}"),
        IR::Call(raw_name) => format!("call {raw_name}"),
        IR::GlobalGet(raw_name) => format!("global.get {raw_name}"),
        IR::GlobalSet(raw_name) => format!("global.set {raw_name}"),
        IR::LocalGet(raw_name) => format!("local.get {raw_name}"),
        IR::LocalSet(raw_name) => format!("local.set {raw_name}"),

        _ => format!("[[[{}, {:#?}]]]", typ, val)
    }
}
