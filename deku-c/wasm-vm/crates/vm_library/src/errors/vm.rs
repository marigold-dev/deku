use std::fmt::Debug;
use thiserror::Error;

use crate::ticket_table;

use super::ffi::FFIError;

#[derive(Error, Debug)]
#[non_exhaustive]
pub enum VmError {
    #[error("Panic: {0}")]
    Panic(String),
    #[error("Error in host/sandbox ffi: {source}")]
    FFIError {
        #[from]
        source: FFIError,
    },
    #[error("Error compiling Wasm: {0}")]
    CompileErr(String),
    #[error("Error deserializing module: {0}")]
    DeserializeErr(String),
    #[error("Ran out of gas during contract execution")]
    OutOfGas,
    #[error("Generic error: {0}")]
    GenericErr(String),
    #[error("Error instantiating a Wasm module: {0}")]
    InstantiationErr(String),
    #[error("Error resolving Wasm function: {0}")]
    ExternErr(String),
    #[error(
        "Unexpected number of result values when calling '{}'. Expected: {}, actual: {}.",
        function_name,
        expected,
        actual
    )]
    ResultMismatch {
        function_name: String,
        expected: usize,
        actual: usize,
    },
    #[error("Error executing: {0}")]
    RuntimeErr(String),
}

impl From<wasmer::ExportError> for VmError {
    fn from(original: wasmer::ExportError) -> Self {
        VmError::ExternErr(format!("Could not get export: {}", original))
    }
}

impl From<wasmer::RuntimeError> for VmError {
    fn from(original: wasmer::RuntimeError) -> Self {
        let message = format!("RuntimeError: {}", original.message());
        VmError::RuntimeErr(format!("Wasmer runtime error: {}", &message))
    }
}

impl From<wasmer::CompileError> for VmError {
    fn from(original: wasmer::CompileError) -> Self {
        VmError::CompileErr(format!("Could not compile: {}", original))
    }
}
impl From<wasmer::DeserializeError> for VmError {
    fn from(original: wasmer::DeserializeError) -> Self {
        VmError::DeserializeErr(format!("Could not deserialzie: {}", original))
    }
}

impl From<std::convert::Infallible> for VmError {
    fn from(_original: std::convert::Infallible) -> Self {
        unreachable!();
    }
}

impl From<VmError> for wasmer::RuntimeError {
    fn from(original: VmError) -> wasmer::RuntimeError {
        let msg: String = original.to_string();
        wasmer::RuntimeError::new(msg)
    }
}
impl From<ticket_table::Error> for VmError {
    fn from(original: ticket_table::Error) -> VmError {
        let msg: String = original.to_string();
        VmError::RuntimeErr(msg)
    }
}
