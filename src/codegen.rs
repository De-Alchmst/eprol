use crate::{
    ir::*,
};
use std::{
    collections::{HashMap},
    sync::{Mutex, OnceLock},
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


pub fn print_memory(pages: i32, import: Vec<&str>) {
    let import_str = if import.len() > 0 {
        format!(" (import {})",
                import.iter().map(|s| format!("\"{}\"", s))
                      .collect::<Vec<String>>().join(" "))
    } else {
        String::from("")
    };
    println!(" (memory {} {})", import_str, pages);
}


static DATA_INFO: OnceLock<Mutex<HashMap<String, u32>>> = OnceLock::new();
fn get_data_info<'a>() -> &'static Mutex<HashMap<String, u32>> {
    DATA_INFO.get_or_init(|| Mutex::new(HashMap::new()))
}

fn u32_to_hex_escapes(value: u32) -> String {
    value.to_le_bytes().iter().map(|b| format!("\\{:02x}", b)).collect()
}

// TODO: escape strings
pub fn print_data(mut data_top: u32, lst: Vec<String>) {
    let mut data_info = get_data_info().lock().unwrap();

    for s in lst {
        let length = s.len() as u32;
        println!(" (data (i32.const {}) \"{}{}\")",
                 data_top, u32_to_hex_escapes(length), s);
        // + 4 for length data
        data_info.insert(s, data_top + 4);
        data_top += length + 4;
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
                println!("\n (func {}{} {} {}\n   {}",
                         raw_name, export,
                         args.iter().map(|(name, typ)|
                             format!("(param {} {})", name, typ))
                             .collect::<Vec<String>>().join(" "),
                         irtype_to_return(ret_type),
                         locals.iter()
                               .map(|(name, typ)| format!("(local {name} {typ})"))
                               .collect::<Vec<String>>().join("\n   "));

                for ir in body {
                    println!("   {}", ir_to_str(ir))
                }
                println!(" )")
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
        IR::Add => format!("{typ}.add"),

        IR::LitStr(s) =>
            format!("i32.const {}",
                    get_data_info().lock().unwrap().get(s).unwrap_or(&0)),

        _ => format!("[[[{}, {:#?}]]]", typ, val)
    }
}
