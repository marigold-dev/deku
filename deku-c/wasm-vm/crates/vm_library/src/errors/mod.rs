use self::vm::VmError;

use ffi::FFIError;

pub mod ffi;
pub mod vm;
pub type FFIResult<T> = Result<T, FFIError>;
pub type VMResult<T> = Result<T, VmError>;
