open Bench_utils
open Deku_tests
open Patricia_tests

let benchmark_patricia () =
  let list_bench =
    [
      ("add_find", (fun () -> ignore (test_add_and_find ())), ());
      ("hash_tree", (fun () -> ignore (test_hash_tree ())), ());
      ("hash_values", (fun () -> ignore (test_hash_values ())), ());
    ] in
  bench_throughput_latency "Benchmark Patricia" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L
