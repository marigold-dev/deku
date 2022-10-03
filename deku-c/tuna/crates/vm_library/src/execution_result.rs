use crate::managed::value::Value;

pub struct ExecutionResult {
    pub new_storage: Value,
    pub ops: Value,
    pub remaining_gas: u64,
}
