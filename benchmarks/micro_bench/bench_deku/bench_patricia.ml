open Core_bench
open Deku_tests
open Patricia_tests

(* This bench call [find] twice: in the [tree] and [find n tree] *)
let bench_add_and_find =
  Bench.Test.create ~name:"add and find tree" (fun () ->
      let _ = test_add_and_find () in
      ())

let bench_hash_tree =
  Bench.Test.create ~name:"hash tree" (fun () ->
      let _ = test_hash_tree () in
      ())

let bench_hash_values =
  Bench.Test.create ~name:"hash values" (fun () ->
      let _ = test_hash_values () in
      ())

let tests = [bench_add_and_find; bench_hash_tree; bench_hash_values]

let command = Bench.make_command tests
