use std::{cell::RefCell, rc::Rc};

use wasmer::Module;

use crate::{managed::value::Value, path::Path, ticket_table::TicketTable};
pub struct InvokeManaged<'a> {
    pub table: Rc<RefCell<TicketTable>>,
    pub mod_: &'a Module,
    pub entrypoint_path: &'a Option<Vec<Path>>,
    pub arg: Value,
    pub initial_storage: Box<Value>,
    pub constants: &'a [(u32, Value)],
    pub source: String,
    pub sender: String,
    pub self_addr: String,
    pub gas_limit: u64,
}
