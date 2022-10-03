use wasmer::Module;

use crate::{managed::value::Value, path::Path, ticket_table::Ticket};
pub struct InvokeManaged<'a> {
    pub mod_: &'a Module,
    pub entrypoint_path: &'a Option<Vec<Path>>,
    pub arg: Value,
    pub initial_storage: Value,
    pub constants: Vec<(i32, Value)>,
    pub tickets: &'a [Ticket],
    pub source: String,
    pub sender: String,
    pub self_addr: String,
    pub gas_limit: u64,
}
