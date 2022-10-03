#![warn(
    clippy::perf,
    clippy::complexity,
    clippy::correctness,
    clippy::todo,
    clippy::needless_continue,
    clippy::needless_borrow,
    clippy::if_let_mutex,
    clippy::suboptimal_flops,
    clippy::lossy_float_literal,
    clippy::fn_params_excessive_bools,
    clippy::inefficient_to_string,
    clippy::macro_use_imports,
    clippy::option_option,
    clippy::unnested_or_patterns,
    clippy::str_to_string,
    clippy::cast_lossless,
    clippy::implicit_clone,
    clippy::redundant_closure_for_method_calls
)]
pub mod arena;
pub mod compile;
pub mod compile_store;
pub mod contract_address;
pub(crate) mod conversions;
pub mod env;
pub mod errors;
pub mod execution_result;
pub mod incoming;
pub mod instance;
pub mod managed;
pub mod outgoing;
pub mod path;
pub mod pipe;
pub mod run_loop;
pub mod state;
pub mod ticket_table;
pub mod vm_client;
pub mod vm_server;
