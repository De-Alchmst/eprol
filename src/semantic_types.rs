use crate::{
    ir::IRType,
    ast::{
        Ident,
        Type,
    }
};

use std::{
    collections::{HashMap, HashSet},
    sync::{Mutex, OnceLock},
};


// types used for Scope representation
#[derive(Clone, Debug)]
pub enum LitVal { Int(i64), Float(f64), Str(String) }
#[derive(Clone, Debug)]
pub enum ScopeItem {
    Var(String, IRType, bool), // raw_name, type, localp
    Const(LitVal), // value
    Accessor(Type, i64), // type, offset in bytes
    Proc(String, Vec<IRType>, IRType), // raw_name, arg types, return type
    None, // used as a return value
}

#[derive(Clone)]
pub struct Scope {
    pub contents: HashMap<String, ScopeItem>,
    pub namespaces: HashMap<String, Scope>,
}

// GLOBAL STATE //

// cache for already processed files imported at multiple places
static _FILE_CACHE: OnceLock<Mutex<HashMap<&str, Scope>>> = OnceLock::new();
pub fn _get_file_cache<'a>() -> &'static Mutex<HashMap<&'a str, Scope>> {
    _FILE_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

// used to store data for the wasm data section
// currently only strings
static DATA_SET: OnceLock<Mutex<HashSet<String>>> = OnceLock::new();
pub fn get_data_set<'a>() -> &'static Mutex<HashSet<String>> {
    DATA_SET.get_or_init(|| Mutex::new(HashSet::new()))
}

impl Scope {
    pub fn search(&self, ident: &Ident) -> ScopeItem {
        let mut current_scope = self;

        // step through namespaces to find required scope, return Error if not found
        for i in 0..ident.namespace.len() {
            match current_scope.namespaces.get(ident.namespace[i]) {
                Some(ns) => {
                    current_scope = &ns;
                }
                None => return ScopeItem::None
            }
        }
        if let Some(var) = current_scope.contents.get(ident.name) {
            var.clone()
        } else {
            ScopeItem::None
        }
    }


    pub fn insert(&mut self, ident: &Ident, item: ScopeItem) {
        let mut current_scope = self;

        // step through namespaces to find required scope, create if not found
        for i in 0..ident.namespace.len() {
            if !current_scope.namespaces.contains_key(ident.namespace[i]) {
                current_scope.namespaces.insert(ident.namespace[i].to_string(), Scope {
                    contents: HashMap::new(),
                    namespaces: HashMap::new(),
                });
            }
            current_scope = current_scope.namespaces.get_mut(ident.namespace[i])
                                                    .unwrap();
        }
        current_scope.contents.insert(ident.name.to_string(), item);
    }
}

