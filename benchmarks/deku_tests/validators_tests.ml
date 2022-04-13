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

(***************************************************************)
(* [current] function  *)

let test_current_validator () =
  let validators, _validator_1 = setup_one () in
  current validators

(***************************************************************)
(* [to_list] function  *)

let test_to_list () =
  let validators, _validator_1 = setup_one () in
  to_list validators

(***************************************************************)
(* [length] function  *)

let test_length () =
  let validators, _validator_1 = setup_one () in
  length validators

(***************************************************************)
(* [remove] function  *)

let test_remove () =
  let validators, validator_1, _validator_2 = setup_two () in
  validators |> remove validator_1

(***************************************************************)
(* [after_current] function  *)

let test_after_current () = after_current 0 empty

(***************************************************************)
(* [update_current] function  *)

let test_update_current () =
  let validators, _validator_1, validator_2 = setup_two () in
  update_current validator_2.address validators

(***************************************************************)
(* [hash] function  *)

let test_hash () =
  let validators, _validator_1, _validator_2 = setup_two () in
  hash validators
