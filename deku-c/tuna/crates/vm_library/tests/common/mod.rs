use serde::{Deserialize, Serialize};
use vm_library::{
    compile_store, incoming::InvokeManaged, managed::value::Value, path::Path, ticket_table::Ticket,
};
use wasmer::{wat2wasm, Module};
pub fn deser(s: String) -> (Init, Module) {
    let deser: Init = serde_json::from_str(&s).unwrap();
    let mod_ = Module::new(
        &compile_store::new_compile_store(),
        wat2wasm(deser.module_.as_bytes()).unwrap(),
    )
    .unwrap();
    (deser, mod_)
}
pub fn create_incoming_managed<'a>(
    mode: &'a Module,
    deser: &'a Init,
    tickets: &'a [Ticket],
    arg: Value,
    initial_storage: Value,
    entrypoint_path: &'a Option<Vec<Path>>,
) -> InvokeManaged<'a> {
    InvokeManaged {
        mod_: mode,
        arg,
        initial_storage,
        constants: deser.constants.clone(),
        tickets,
        entrypoint_path,
        source: "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string(),
        sender: "tz1gvF4cD2dDtqitL3ZTraggSR1Mju2BKFEM".to_string(),
        self_addr: "KT1WiBZHtvv3EczaN628DkNob4cayHzTEDNK".to_string(),
        gas_limit: u64::MAX,
    }
}

#[derive(Deserialize, Serialize)]
pub struct Init {
    module_: String,
    constants: Vec<(i32, Value)>,
}
