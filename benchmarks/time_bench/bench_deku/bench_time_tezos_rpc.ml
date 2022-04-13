open Bench_utils
open Deku_tests
open Tezos_rpc_tests

let benchmark_tezos_rpc () =
  let list_bench =
    [
      ( "fetch block ops",
        (fun () -> ignore (fetch_block_operations ~block_hash:None)),
        () );
      ( "fetch block header",
        (fun () -> ignore (fetch_block_header ~block_hash:None)),
        () );
      ( "fetch constants",
        (fun () -> ignore (fetch_constants ~block_hash:None)),
        () );
    ] in
  bench_throughput_latency "Benchmark Tezos rpc" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L
