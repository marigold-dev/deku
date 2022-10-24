use crate::managed::value::Value;

pub struct ExecutionResult {
    pub new_storage: Box<Value>,
    pub ops: Box<Value>,
    pub remaining_gas: u64,
}
