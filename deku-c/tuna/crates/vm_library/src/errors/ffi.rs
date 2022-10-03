use crate::managed::value::Value;
use std::fmt::Debug;
use thiserror::Error;
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum FFIError {
    #[error(
        "Wrong type of extern {:?} provided by the contract for the ffi call, {} ",
        value,
        msg
    )]
    ExternError { value: Value, msg: String },
}
