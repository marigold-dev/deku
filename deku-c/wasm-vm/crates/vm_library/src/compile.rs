use wasmer::{wat2wasm, Module};

use crate::{
    compile_store::new_compile_store,
    errors::{vm::VmError, VMResult},
};

pub fn compile_managed_module(m: &[u8]) -> VMResult<Module> {
    wasmer::Module::new(
        &new_compile_store(),
        wat2wasm(m).map_err(|_| VmError::CompileErr("failed to compile module".to_owned()))?,
    )
    .map_err(std::convert::Into::into)
}
