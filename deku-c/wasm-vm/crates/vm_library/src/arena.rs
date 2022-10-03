use fnv::FnvHashMap;
use im_rc::{ordmap, ordset, vector};
use slotmap::{DefaultKey, HopSlotMap};

use crate::{
    managed::value::Value,
    ticket_table::{Ticket, TicketId, TicketTable},
};
use once_cell::unsync::Lazy;

pub static mut ARENA: Lazy<HopSlotMap<DefaultKey, Value>> =
    Lazy::new(|| HopSlotMap::with_capacity(4000));

use std::collections::BTreeMap;
pub static mut PREDEF: Lazy<FnvHashMap<String, Value>> =
    Lazy::new(|| FnvHashMap::with_capacity_and_hasher(16, Default::default()));
pub static mut CONSTANTS: Lazy<Vec<Value>> = Lazy::new(|| Vec::with_capacity(3000));
pub static mut TICKETS: Lazy<BTreeMap<usize, Ticket>> = Lazy::new(BTreeMap::new);
pub static mut INVERSETICKETS: Lazy<FnvHashMap<Ticket, usize>> =
    Lazy::new(|| FnvHashMap::with_capacity_and_hasher(100, Default::default()));
pub static mut TICKETABLE: Lazy<TicketTable> = Lazy::new(TicketTable::default);
pub static mut CONSUMEDTICKETS: Lazy<Vec<(TicketId, usize)>> =
    Lazy::new(|| Vec::with_capacity(3000));

pub fn populate_predef(sender: String, self_: String, source: String) {
    let map = unsafe { &mut PREDEF };
    map.clear();
    map.insert("none".to_owned(), Value::Option(None));
    map.insert("unit".to_owned(), Value::Unit);
    map.insert("true".to_owned(), Value::Bool(true));
    map.insert("false".to_owned(), Value::Bool(false));
    map.insert("nil".to_owned(), Value::List(vector![], None));
    map.insert("source".to_owned(), Value::String(source));
    map.insert("sender".to_owned(), Value::String(sender));
    map.insert("self".to_owned(), Value::String(self_));
    map.insert("empty_set".to_owned(), Value::Set(ordset![]));
    map.insert("empty_map".to_owned(), Value::Map(ordmap! {}));
    map.insert("zero".to_owned(), Value::Int(0.into()));
}
pub fn push_constants(vec: Vec<(i32, Value)>) {
    let map = unsafe { &mut CONSTANTS };
    map.clear();
    vec.into_iter().for_each(|(k, v)| {
        map.insert(k as usize, v);
    })
}
