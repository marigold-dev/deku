open Deku_concepts
open Deku_crypto
open Deku_stdlib
open Deku_protocol

let alice_secret =
  Secret.of_b58 "edsk3EYk77QNx9HM4YDh5rv5nzBL68z2YGtBUGXhkw3rMhB2eCNvHf"
  |> Option.get

let alice = Identity.make alice_secret

let bob_secret =
  Secret.of_b58 "edsk4Qejwxwj7JD93B45gvhYHVfMzNjkBWRQDaYkdt5JcUWLT4VDkh"
  |> Option.get

let bob = Identity.make bob_secret

(* helper to create in an easy way an operation that transfer n token from alice to bob *)
let make_operation ?(nonce = 1) ?(level = 0) ?(amount = 0) () =
  let level = Level.of_n (N.of_z (Z.of_int level) |> Option.get) in
  let nonce = Nonce.of_n (N.of_z (Z.of_int nonce) |> Option.get) in
  let amount = Amount.of_n (N.of_z (Z.of_int amount) |> Option.get) in
  let operation =
    Operation.transaction ~identity:alice ~level ~nonce
      ~source:(Address.of_key_hash (Identity.key_hash alice))
      ~receiver:(Address.of_key_hash (Identity.key_hash bob))
      ~amount
  in
  let operation_str =
    operation |> Operation.yojson_of_t |> Yojson.Safe.to_string
  in
  let (Operation.Operation { hash; _ }) = operation in
  (operation, operation_str, hash)

(* The parallel function given to the Protocol.apply *)
let parallel = List.filter_map

let test_apply_one_operation () =
  let _, op_str, _ = make_operation () in
  let _, receipts =
    Protocol.initial
    |> Protocol.apply ~parallel ~current_level:Level.zero ~payload:[ op_str ]
  in
  Alcotest.(check bool) "operation is included" true (List.length receipts = 1)

let run () =
  let open Alcotest in
  run "Protocol" ~and_exit:false
    [
      ( "apply operations",
        [ test_case "apply one operation" `Quick test_apply_one_operation ] );
    ]
