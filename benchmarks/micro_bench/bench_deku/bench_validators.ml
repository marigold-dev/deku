open Core_bench
open Deku_tests
open Validators_tests
let bench_setup_one =
  Bench.Test.create ~name:"setup one validator" (fun () ->
      let _ = setup_one () in
      ())

let bench_setup_two =
  Bench.Test.create ~name:"setup two validators" (fun () ->
      let _ = setup_two () in
      ())

let bench_make_validator =
  Bench.Test.create ~name:"make validator" (fun () ->
      let _ = make_validator () in
      ())

(***************************************************************)
(* [current] function  *)

let bench_current_validator =
  Bench.Test.create ~name:"current validator" (fun () ->
      let _ = test_current_validator () in
      ())

(***************************************************************)
(* [to_list] function  *)

let bench_to_list =
  Bench.Test.create ~name:"to_list" (fun () ->
      let _ = test_to_list () in
      ())

(***************************************************************)
(* [length] function  *)

let bench_length =
  Bench.Test.create ~name:"length" (fun () ->
      let _ = test_length () in
      ())

(***************************************************************)
(* [remove] function  *)

let bench_remove_validator =
  Bench.Test.create ~name:"remove validator" (fun () ->
      let _ = test_remove () in
      ())

(***************************************************************)
(* [after_current] function  *)

let bench_after_current =
  Bench.Test.create ~name:"after_current" (fun () ->
      let _ = test_after_current () in
      ())

(***************************************************************)
(* [update_current] function  *)

let bench_update_current =
  Bench.Test.create ~name:"update_current" (fun () ->
      let _ = test_update_current () in
      ())

(***************************************************************)
(* [hash] function  *)

let bench_hash =
  Bench.Test.create ~name:"hash" (fun () ->
      let _ = test_hash () in
      ())

let tests =
  [
    bench_make_validator;
    bench_setup_one;
    bench_setup_two;
    bench_current_validator;
    bench_to_list;
    bench_length;
    bench_remove_validator;
    bench_after_current;
    bench_update_current;
    bench_hash;
  ]

let command = Bench.make_command tests
