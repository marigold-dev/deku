use mimalloc::MiMalloc;
use vm_library::{pipe::IO, run_loop::run_loop};
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;
fn main() {
    env_logger::init();
    let mut args: Vec<String> = std::env::args().collect();
    let io = IO::new(args.remove(1));
    run_loop(io);
}
