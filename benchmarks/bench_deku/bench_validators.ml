open Core_bench
open Protocol
open Validators

let make_validator () =
  let open Crypto in
  let _key, wallet = Ed25519.generate () in
  let address = Key_hash.of_key (Ed25519 wallet) in
  let open Validators in
  { address }

(***************************************************************)
(* [empty], [add] functions  *)

let setup_one () =
  let validator_1 = make_validator () in
  let validators = empty |> add validator_1 in
  (validators, validator_1)

let setup_two () =
  let validator_1 = make_validator () in
  let validator_2 = make_validator () in
  let validators = empty |> add validator_1 |> add validator_2 in
  (validators, validator_1, validator_2)

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
      let validators, _validator_1 = setup_one () in
      let _ = current validators in
      ())

(***************************************************************)
(* [to_list] function  *)

let bench_to_list =
  Bench.Test.create ~name:"to_list" (fun () ->
      let validators, _validator_1 = setup_one () in
      let _ = to_list validators in
      ())

(***************************************************************)
(* [length] function  *)

let bench_length =
  Bench.Test.create ~name:"length" (fun () ->
      let validators, _validator_1 = setup_one () in
      let _ = length validators in
      ())

(***************************************************************)
(* [remove] function  *)

let bench_remove_validator =
  Bench.Test.create ~name:"remove validator" (fun () ->
      let validators, validator_1, _validator_2 = setup_two () in
      let _validators = validators |> remove validator_1 in
      ())

(***************************************************************)
(* [after_current] function  *)

let bench_after_current =
  Bench.Test.create ~name:"after_current" (fun () ->
      let _ = after_current 0 empty in
      ())

(***************************************************************)
(* [update_current] function  *)

let bench_update_current =
  Bench.Test.create ~name:"update_current" (fun () ->
      let validators, _validator_1, validator_2 = setup_two () in
      let _ = update_current validator_2.address validators in
      ())

(***************************************************************)
(* [hash] function  *)

let bench_hash =
  Bench.Test.create ~name:"hash" (fun () ->
      let validators, _validator_1, _validator_2 = setup_two () in
      let _ = hash validators in
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
