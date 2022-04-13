open Bench_utils
open Deku_tests
open Validators_tests

let benchmark_validators () =
  let list_bench =
    [
      ("setup", (fun () -> ignore (setup_one ())), ());
      ("setup two", (fun () -> ignore (setup_two ())), ());
      ("make", (fun () -> ignore (make_validator ())), ());
      ("current", (fun () -> ignore (test_current_validator ())), ());
      ("to_list", (fun () -> ignore (test_to_list ())), ());
      ("length", (fun () -> ignore (test_length ())), ());
      ("remove", (fun () -> ignore (test_remove ())), ());
      ("after_current", (fun () -> ignore (test_after_current ())), ());
      ("update_current", (fun () -> ignore (test_update_current ())), ());
      ("hash", (fun () -> ignore (test_hash ())), ());
    ] in
  bench_throughput_latency "Benchmark validators " ~repeat:5 ~time:10 list_bench
    ~latency:20_000L
