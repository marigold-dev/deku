open Deku_concepts
open Deku_crypto
open Deku_stdlib
open Deku_protocol
open Deku_constants

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

let test_many_operations () =
  (* Creates a list of 10 different operations *)
  let operations, payload, op_hashes =
    List.init 10 (fun nonce -> make_operation ~nonce ())
    |> List.fold_left
         (fun (ops, payload, hashes) (op, op_str, op_hash) ->
           (op :: ops, op_str :: payload, op_hash :: hashes))
         ([], [], [])
  in
  let protocol, receipts =
    Protocol.initial
    |> Protocol.apply ~parallel ~current_level:Level.zero ~payload
  in
  let (Protocol.Protocol { included_operations; _ }) = protocol in
  let all_ops_are_included =
    operations
    |> List.for_all (fun op ->
           Included_operation_set.mem op included_operations)
  in
  let receipts_are_op_hashes =
    receipts
    |> List.for_all (fun (Receipt.Receipt { operation; _ }) ->
           List.find_opt
             (fun hash -> Operation_hash.equal hash operation)
             op_hashes
           |> Option.is_some)
  in
  Alcotest.(check bool)
    "there should be 10 receipts" true
    (List.length receipts = 10);
  Alcotest.(check bool)
    "the 10 operations should be included" true all_ops_are_included;
  Alcotest.(check bool)
    "the receipts correspond to the given operation hashes" true
    receipts_are_op_hashes

let test_duplicated_operation_same_level () =
  let _, op_str, hash = make_operation () in

  let _, receipts =
    Protocol.initial
    |> Protocol.apply ~parallel ~current_level:Level.zero
         ~payload:[ op_str; op_str ]
  in
  let (Receipt.Receipt { operation }) = List.hd receipts in
  Alcotest.(check bool)
    "there should only be one receipt" true
    (List.length receipts = 1);
  Alcotest.(check bool)
    "the hash correspond" true
    (Operation_hash.equal operation hash)

let test_duplicated_operation_different_level () =
  let _, op_str, _ = make_operation () in
  let _, receipts =
    Protocol.initial
    |> Protocol.apply ~parallel ~current_level:Level.zero ~payload:[ op_str ]
    |> fst
    |> Protocol.apply ~parallel ~current_level:(Level.next Level.zero)
         ~payload:[ op_str ]
  in
  Alcotest.(check bool)
    "second operation shouldn't be applied" true
    (List.length receipts = 0)

let test_duplicated_operation_after_includable_window () =
  let _, op_str, _ = make_operation () in
  let _, receipts =
    Protocol.initial
    |> Protocol.apply ~parallel ~current_level:Level.zero ~payload:[ op_str ]
    |> fst
    |> Protocol.apply ~parallel
         ~current_level:(Level.of_n N.(zero + includable_operation_window))
         ~payload:[]
    |> fst
    |> Protocol.apply ~parallel
         ~current_level:
           (Level.of_n N.(zero + includable_operation_window + one))
         ~payload:[ op_str ]
  in
  Alcotest.(check bool)
    "operation shouldn't be applied" true
    (List.length receipts = 0)

let test_invalid_string () =
  let wrong_op_str = "waku waku" in
  let _, receipts =
    Protocol.initial
    |> Protocol.apply ~parallel ~current_level:Level.zero
         ~payload:[ wrong_op_str ]
  in
  Alcotest.(check bool) "shouldn't be included" true (List.length receipts = 0)

let run () =
  let open Alcotest in
  run "Protocol" ~and_exit:false
    [
      ( "apply operations",
        [
          test_case "apply one operation" `Quick test_apply_one_operation;
          test_case "apply many operations" `Quick test_many_operations;
          test_case "duplicated operation on same level" `Quick
            test_duplicated_operation_same_level;
          test_case "duplicated operation on different level" `Quick
            test_duplicated_operation_different_level;
          test_case "duplicated operation after includable window" `Quick
            test_duplicated_operation_after_includable_window;
          test_case "invalid string" `Quick test_invalid_string;
        ] );
    ]
