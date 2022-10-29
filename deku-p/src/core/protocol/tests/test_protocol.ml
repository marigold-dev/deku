open Deku_concepts
open Deku_crypto
open Deku_stdlib
open Deku_protocol
open Deku_constants
open Deku_ledger

let alice_secret =
  Secret.of_b58 "edsk3EYk77QNx9HM4YDh5rv5nzBL68z2YGtBUGXhkw3rMhB2eCNvHf"
  |> Option.get

let alice = Identity.make alice_secret

let bob_secret =
  Secret.of_b58 "edsk4Qejwxwj7JD93B45gvhYHVfMzNjkBWRQDaYkdt5JcUWLT4VDkh"
  |> Option.get

let bob = Identity.make bob_secret

let ticket_id =
  let address =
    Deku_tezos.Contract_hash.of_b58 "KT1JQ5JQB4P1c8U8ACxfnodtZ4phDVMSDzgi"
    |> Option.get
  in
  let data = Bytes.of_string "" in
  Ticket_id.make (Tezos address) data

(* helper to create in an easy way an operation that transfer n token from alice to bob *)
let make_operation ?(nonce = 1) ?(level = 0) ?(amount = 0) () =
  let level = Level.of_n (N.of_z (Z.of_int level) |> Option.get) in
  let nonce = Nonce.of_n (N.of_z (Z.of_int nonce) |> Option.get) in
  let amount = Amount.of_n (N.of_z (Z.of_int amount) |> Option.get) in
  let operation =
    Operation.Signed.ticket_transfer ~identity:alice ~level ~nonce
      ~receiver:(Address.of_key_hash (Identity.key_hash bob))
      ~ticket_id ~amount
  in
  let operation_str =
    Data_encoding.Binary.to_string_exn Operation.Signed.encoding operation
  in
  let (Signed_operation { initial = Initial_operation { hash; _ }; _ }) =
    operation
  in
  (operation, operation_str, hash)

(* The parallel function given to the Protocol.apply *)
let parallel = List.filter_map

let assert_all_were_applied_with_receipts ~operations ~protocol ~receipts =
  let (Protocol.Protocol { included_operations; _ }) = protocol in
  let operations =
    List.map
      (fun (Operation.Signed.Signed_operation { initial; _ }) -> initial)
      operations
  in
  let all_ops_are_included =
    operations
    |> List.for_all (fun op ->
           Included_operation_set.mem op included_operations)
  in
  let op_hashes =
    List.map
      (fun (Operation.Initial.Initial_operation { hash; _ }) -> hash)
      operations
    |> List.sort Operation_hash.compare
  in
  let[@warning "-8"] op_hashes_from_receipts =
    List.map
      (fun (Receipt.Ticket_transfer_receipt { operation; _ }) -> operation)
      receipts
    |> List.sort Operation_hash.compare
  in
  let all_receipts_have_hashes_and_vice_versa =
    List.equal Operation_hash.equal op_hashes op_hashes_from_receipts
  in
  all_receipts_have_hashes_and_vice_versa && all_ops_are_included

let test_apply_one_operation () =
  let op, op_str, _ = make_operation () in
  let protocol, receipts, _errors =
    let payload = Protocol.prepare ~parallel ~payload:[ op_str ] in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  Alcotest.(check bool)
    "operation is included" true
    (assert_all_were_applied_with_receipts ~operations:[ op ] ~receipts
       ~protocol)

let test_many_operations () =
  (* Creates a list of 10 different operations *)
  let operations, payload, _op_hashes =
    List.init 10 (fun nonce -> make_operation ~nonce ())
    |> List.fold_left
         (fun (ops, payload, hashes) (op, op_str, op_hash) ->
           (op :: ops, op_str :: payload, op_hash :: hashes))
         ([], [], [])
  in
  let protocol, receipts, _errors =
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  Alcotest.(check bool)
    "all operations have receipts and vice versa" true
    (assert_all_were_applied_with_receipts ~operations ~protocol ~receipts)

let test_duplicated_operation_same_level () =
  let _, op_str, hash = make_operation () in

  let _, receipts, _errors =
    let payload = Protocol.prepare ~parallel ~payload:[ op_str; op_str ] in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  let[@warning "-8"] (Receipt.Ticket_transfer_receipt { operation; _ }) =
    List.hd receipts
  in
  Alcotest.(check bool)
    "there should only be one receipt" true
    (List.length receipts = 1);
  Alcotest.(check bool)
    "the hash correspond" true
    (Operation_hash.equal operation hash)

(* TODO: we can collapse a lot of these properties into
   property based tests. We want to check that
   - any time an operation is included, the corresponding receipt is generated
   - receipts are never generated otherwise
   - plus all the properties we're testing here.
*)
let test_duplicated_operation_different_level () =
  let _, op_str, _ = make_operation () in
  let _, receipts, _errors =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    let protocol, _, _errors =
      Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
        Protocol.initial
    in
    Protocol.apply ~current_level:(Level.next Level.zero) ~payload
      ~tezos_operations:[] protocol
  in
  Alcotest.(check bool)
    "second operation shouldn't be applied" true
    (List.length receipts = 0)

let test_duplicated_operation_after_includable_window () =
  let _, op_str, _ = make_operation () in
  let protocol, _receipts, _errors =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  (* TODO: should we have an integrity check in Protocol.apply that checks
     that the block being applied is a valid next block? *)
  let protocol, _recipets, _errors =
    Protocol.apply
      ~current_level:(Level.of_n N.(zero + includable_operation_window))
      ~tezos_operations:[] ~payload:[] protocol
  in
  let _protocol, receipts, _errors =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply
      ~current_level:(Level.of_n N.(zero + includable_operation_window + one))
      ~payload protocol ~tezos_operations:[]
  in
  Alcotest.(check bool)
    "operation shouldn't be applied" true
    (List.length receipts = 0)

let test_invalid_string () =
  let wrong_op_str = "waku waku" in
  let _, receipts, _errors =
    let payload = [ wrong_op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  Alcotest.(check bool) "shouldn't be included" true (List.length receipts = 0)

let test_invalid_signature () =
  let operation =
    `Assoc
      [
        ("level", Level.yojson_of_t Level.zero);
        ("nonce", Nonce.yojson_of_t (Nonce.of_n N.one));
        ( "source",
          Address.yojson_of_t (Address.of_key_hash (Identity.key_hash alice)) );
        ( "content",
          `List
            [
              `String "Transaction";
              `Assoc
                [
                  ( "receiver",
                    Address.yojson_of_t
                      (Address.of_key_hash (Identity.key_hash bob)) );
                  ("amount", Amount.yojson_of_t Amount.zero);
                ];
            ] );
      ]
  in
  let hash = BLAKE2b.hash "waku waku" in
  let signature = Identity.sign ~hash alice in
  let json =
    `Assoc
      [
        ("key", Key.yojson_of_t (Identity.key alice));
        ("signature", Signature.yojson_of_t signature);
        ("operation", operation);
      ]
  in
  let op_str = Yojson.Safe.to_string json in
  let _, receipts, _errors =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  Alcotest.(check bool) "shouldn't be included" true (List.length receipts = 0)

let test_valid_signature_but_different_key () =
  let operation =
    `Assoc
      [
        ("level", Level.yojson_of_t Level.zero);
        ("nonce", Nonce.yojson_of_t (Nonce.of_n N.one));
        ( "source",
          Address.yojson_of_t (Address.of_key_hash (Identity.key_hash alice)) );
        ( "content",
          `List
            [
              `String "Transaction";
              `Assoc
                [
                  ( "receiver",
                    Address.yojson_of_t
                      (Address.of_key_hash (Identity.key_hash bob)) );
                  ("amount", Amount.yojson_of_t Amount.zero);
                ];
            ] );
      ]
  in
  let hash =
    Yojson.Safe.to_string operation
    |> Operation_hash.hash |> Operation_hash.to_blake2b
  in
  let signature = Identity.sign ~hash alice in
  let json =
    `Assoc
      [
        ("key", Key.yojson_of_t (Identity.key bob));
        ("signature", Signature.yojson_of_t signature);
        ("operation", operation);
      ]
  in
  let op_str = Yojson.Safe.to_string json in
  let _, receipts, _errors =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  Alcotest.(check bool) "shouldn't be included" true (List.length receipts = 0)

let test_receipt_implied_included_operations () =
  let op, op_str, _ = make_operation () in
  let protocol, receipts, _errors =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      Protocol.initial
  in
  let (Protocol.Protocol { included_operations; _ }) = protocol in
  let (Signed_operation { initial = op; _ }) = op in
  let is_included = Included_operation_set.mem op included_operations in
  Alcotest.(check bool) "the operation is included" true is_included;
  Alcotest.(check bool) "there only one receipt" true (List.length receipts = 1)

let test_included_operation_clean_after_window () =
  let op, op_str, _ = make_operation () in
  let protocol, _receipts, _errors =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    let protocol, _receipts, _errors =
      Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
        Protocol.initial
    in

    Protocol.apply
      ~current_level:(Level.of_n N.(one + includable_operation_window))
      ~payload:[] ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { included_operations; _ }) = protocol in
  let (Signed_operation { initial = op; _ }) = op in
  let is_included = Included_operation_set.mem op included_operations in
  Alcotest.(check bool) "included operations should be empty" false is_included

let amount = Alcotest.testable Amount.pp Amount.equal

let test_cannot_create_amount_ex_nihilo () =
  let _, op_str, _ = make_operation ~amount:32 () in
  let protocol = Protocol.initial in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_previous_balance =
    ledger
    |> Ledger.balance
         (bob |> Identity.key_hash |> Address.of_key_hash)
         ticket_id
  in
  let alice_previous_balance =
    ledger
    |> Ledger.balance
         (alice |> Identity.key_hash |> Address.of_key_hash)
         ticket_id
  in
  let protocol, _, _ =
    let payload = [ op_str ] in
    let payload = Protocol.prepare ~parallel ~payload in
    Protocol.apply ~current_level:Level.zero ~payload ~tezos_operations:[]
      protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance =
    ledger
    |> Ledger.balance
         (bob |> Identity.key_hash |> Address.of_key_hash)
         ticket_id
  in
  let alice_balance =
    ledger
    |> Ledger.balance
         (alice |> Identity.key_hash |> Address.of_key_hash)
         ticket_id
  in
  Alcotest.(check amount)
    "balance of alice has not changed" bob_previous_balance bob_balance;
  Alcotest.(check amount)
    "balance of bob has not changed" alice_previous_balance alice_balance

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
          test_case "invalid signature" `Quick test_invalid_signature;
          test_case "good signature, wrong key" `Quick
            test_valid_signature_but_different_key;
          test_case "one receipt imply one included operation" `Quick
            test_receipt_implied_included_operations;
          test_case "no more included operations after includable window" `Quick
            test_included_operation_clean_after_window;
        ] );
      ( "ledger operation",
        [
          test_case "balance does not change, when not enough amount" `Quick
            test_cannot_create_amount_ex_nihilo;
        ] );
    ]
