use fnv::FnvHashMap;
use im_rc::{ordmap, ordset, vector};
use slotmap::{DefaultKey, SlotMap};

use crate::{managed::value::Value, ticket_table::Ticket};
use once_cell::unsync::Lazy;

pub static mut ARENA: Lazy<SlotMap<DefaultKey, Value>> = Lazy::new(|| SlotMap::with_capacity(2000));

use std::collections::BTreeMap;
pub static mut PREDEF: Lazy<FnvHashMap<String, Value>> = Lazy::new(|| {
    let mut map = FnvHashMap::with_capacity_and_hasher(16, Default::default());
    map.insert("none".to_owned(), Value::Option(None));
    map.insert("unit".to_owned(), Value::Unit);
    map.insert("true".to_owned(), Value::Bool(true));
    map.insert("false".to_owned(), Value::Bool(false));
    map.insert("nil".to_owned(), Value::List(vector![], None));

    map.insert("empty_set".to_owned(), Value::Set(ordset![]));
    map.insert("empty_map".to_owned(), Value::Map(ordmap! {}));
    map.insert("zero".to_owned(), Value::Int(0.into()));
    map
});
pub static mut CONSTANTS: Lazy<Vec<Value>> = Lazy::new(|| Vec::with_capacity(3000));
pub static mut TICKETS: Lazy<BTreeMap<usize, Ticket>> = Lazy::new(BTreeMap::new);

pub fn populate_predef(sender: String, self_: String, source: String) {
    let map = unsafe { &mut PREDEF };

    map.insert("source".to_owned(), Value::String(source));
    map.insert("sender".to_owned(), Value::String(sender));
    map.insert("self".to_owned(), Value::String(self_));
}
pub fn push_constants(vec: &[(u32, Value)]) {
    let map = unsafe { &mut CONSTANTS };
    map.clear();
    vec.iter().for_each(|(k, v)| {
        map.insert(*k as usize, v.clone());
    })
}
