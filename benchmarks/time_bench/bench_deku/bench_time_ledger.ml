open Deku_tests
open Ledger_tests
open Bench_utils

let bench_create () =
  let list_bench =
    [
      ("make_ticket", (fun () -> ignore (make_ticket ())), ());
      ("make_address", (fun () -> ignore (make_address ())), ());
      ("make_tezos_address", (fun () -> ignore (make_tezos_address ())), ());
    ] in
  bench_throughput_latency "Benchmark create" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

(*******************************************************************************)
(* bench function [deposit] *)

let bench_deposit () =
  let list_bench =
    [
      ("deposit", (fun () -> ignore (setup_one_deposit ())), ());
      ("4_deposits", (fun () -> ignore (setup_four_deposits ())), ());
    ] in
  bench_throughput_latency "Deposit" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

(*****************************************************************************)
(* bench function [transfer] without test *)

let bench_transfer () =
  let list_bench =
    [
      ("transfer 1", (fun () -> ignore (test_transfer ())), ());
      ("transfer 4", (fun () -> ignore (test_transfers_4 ())), ());
    ] in
  bench_throughput_latency "Transfer" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

(*****************************************************************************)
(* bench function [withdrawal] without test *)

let bench_withdraw () =
  let list_bench =
    [
      ("withdraw 1", (fun () -> ignore (test_withdraw_1 ())), ());
      ("withdraw 4", (fun () -> ignore (test_withdraw_4 ())), ());
    ] in
  bench_throughput_latency "Withdraw" ~repeat:5 ~time:10 list_bench
    ~latency:20_000L

let benchmark_ledger () =
  bench_create ();
  bench_deposit ();
  bench_transfer ();
  bench_withdraw ()
