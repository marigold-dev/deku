use std::{cell::RefCell, rc::Rc};

use serde::{Deserialize, Serialize};
use vm_library::{
    compile_store,
    incoming::InvokeManaged,
    managed::value::{FromOcamlV, Value},
    path::Path,
    ticket_table::TicketTable,
};
use wasmer::{wat2wasm, Module};
pub fn deser(s: String) -> (Init, Module) {
    let deser: Init2 = serde_json::from_str(&s).unwrap();
    let mod_ = Module::new(
        &compile_store::new_compile_store(),
        wat2wasm(deser.module_.as_bytes()).unwrap(),
    )
    .unwrap();
    let constants = deser
        .constants
        .into_iter()
        .map(|(x, y)| (x, y.0))
        .collect::<Vec<(u32, Value)>>();
    (
        Init {
            constants,
            module_: deser.module_,
        },
        mod_,
    )
}
pub fn create_incoming_managed<'a>(
    mode: &'a Module,
    deser: &'a Init,
    arg: Value,
    initial_storage: Value,
    entrypoint_path: &'a Option<Vec<Path>>,
    table: Rc<RefCell<TicketTable>>,
) -> InvokeManaged<'a> {
    InvokeManaged {
        mod_: mode,
        arg,
        initial_storage: Box::from(initial_storage),
        constants: &deser.constants,
        entrypoint_path,
        source: "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string(),
        sender: "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string(),
        self_addr: "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK".to_string(),
        gas_limit: u64::MAX,
        table,
    }
}

#[derive(Deserialize, Serialize)]
pub struct Init2 {
    module_: String,
    constants: Vec<(u32, FromOcamlV)>,
}
#[derive(Deserialize, Serialize)]
pub struct Init {
    module_: String,
    constants: Vec<(u32, Value)>,
}
