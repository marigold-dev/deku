open Core_bench
open Deku_tests
open Ledger_tests

(* bechmark functions *)

let bench_make_ticket =
  Bench.Test.create ~name:"make ticket" (fun () ->
      let _ = make_ticket () in
      ())

let bench_make_address =
  Bench.Test.create ~name:"make address" (fun () ->
      let _ = make_address () in
      ())

let bench_make_tezos_address =
  Bench.Test.create ~name:"make tezos address" (fun () ->
      let _ = make_tezos_address () in
      ())

(*******************************************************************************)
(* bench function [deposit] *)

let bench_setup_one_deposit =
  Bench.Test.create ~name:"deposit: one ticket, one address" (fun () ->
      let _ = setup_one_deposit () in
      ())

let bench_setup_four_deposits =
  Bench.Test.create ~name:"deposit: two tickets, two addresses" (fun () ->
      let _ = setup_four_deposits () in
      ())

(*****************************************************************************)
(* bench function [balance] with test
   NOTE: do I need to get the bench of tests?
*)

let bench_test_balance =
  Bench.Test.create ~name:"test balance" (fun () -> test_balance ())

(* bench function [balance] with test of:
   - Amount
   - Balance in a setup one deposit *)

let bench_test_amount_get_balance_one_deposit =
  Bench.Test.create ~name:"deposit: test amount, get balance" (fun () ->
      test_balance_deposit ())

(* bench function [balance] with test of:
   - Balance in a setup one deposit *)
let bench_test_get_balance_one_deposit =
  Bench.Test.create ~name:"deposit: test get balance" (fun () ->
      test_balance_one_deposit ())

(* bench function [balance] with test of:
   - Balance in a setup four deposits *)
let bench_test_get_balance_four_deposits =
  Bench.Test.create ~name:"four deposits: test get balance" (fun () ->
      test_balance_four_deposits ())

(*****************************************************************************)
(* bench function [balance] without test *)

let bench_balance_one_deposit =
  Bench.Test.create ~name:"balance: 1 deposit" (fun () ->
      let _ = test_one_deposit () in
      ())

let bench_balance_four_deposits =
  Bench.Test.create ~name:"balance: four deposits" (fun () ->
      let _ = test_four_deposits () in
      ())

(*****************************************************************************)
(* bench function [transfer] without test *)

let bench_transfer_1 =
  Bench.Test.create ~name:"transfer 1" (fun () ->
      let _ = test_transfer () in
      ())

let bench_transfer_4 =
  Bench.Test.create ~name:"transfer 4" (fun () ->
      let _ = test_transfers_4 () in
      ())

(*****************************************************************************)
(* bench function [withdrawal] without test *)

let bench_withdraw_1 =
  Bench.Test.create ~name:"withdraw 1" (fun () ->
      let _ = test_withdraw_1 () in
      ())

let bench_withdraw_4 =
  Bench.Test.create ~name:"withdraw 4" (fun () ->
      let _ = test_withdraw_4 () in
      ())

let tests =
  [
    (* setup *)
    bench_make_ticket;
    bench_make_address;
    bench_make_tezos_address;
    (* deposit *)
    bench_setup_one_deposit;
    bench_setup_four_deposits;
    (* balance *)
    bench_balance_one_deposit;
    bench_balance_four_deposits;
    (* transfer *)
    bench_transfer_1;
    bench_transfer_4;
    (* withdrawal *)
    bench_withdraw_1;
    bench_withdraw_4;
  ]

let command = Bench.make_command tests
