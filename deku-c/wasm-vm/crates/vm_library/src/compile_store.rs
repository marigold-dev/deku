use std::sync::Arc;

use wasmer::{wasmparser::Operator, CompilerConfig, Features, Store, Universal};
use wasmer_compiler_singlepass::Singlepass;
use wasmer_middlewares::Metering;

#[inline]
const fn cost_fn(_operator: &Operator) -> u64 {
    // TODO: find good cost based on benchmarking
    100
}
pub fn new_compile_store() -> Store {
    let metering = Arc::new(Metering::new(0, cost_fn));
    let mut compiler_config = Singlepass::default();
    compiler_config.push_middleware(metering);
    let runtime = Universal::new(compiler_config);
    let mut features = Features::default();
    // //features.module_linking(true);
    features.multi_value(false);
    features.reference_types(true);
    features.bulk_memory(true);
    features.simd(true);
    let runtime = runtime.features(features);
    Store::new(&runtime.engine())
}
pub fn new_headless() -> Store {
    let runtime = Universal::headless();
    let mut features = Features::default();
    // //features.module_linking(true);
    features.multi_value(false);
    features.reference_types(true);
    features.simd(true);
    let runtime = runtime.features(features);
    Store::new(&runtime.engine())
}
