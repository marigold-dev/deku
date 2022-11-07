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
let alice_addr = Address.of_key_hash (Identity.key_hash alice)

let bob_secret =
  Secret.of_b58 "edsk4Qejwxwj7JD93B45gvhYHVfMzNjkBWRQDaYkdt5JcUWLT4VDkh"
  |> Option.get

let bob = Identity.make bob_secret
let bob_addr = Address.of_key_hash (Identity.key_hash bob)

let bob_tezos =
  bob_addr |> Address.to_b58 |> Deku_tezos.Address.of_string |> Option.get

let ticket_id =
  let address =
    Deku_tezos.Contract_hash.of_b58 "KT1JQ5JQB4P1c8U8ACxfnodtZ4phDVMSDzgi"
    |> Option.get
  in
  let data = Bytes.of_string "" in
  Ticket_id.make (Tezos address) data

let amount' = Alcotest.testable Amount.pp Amount.equal

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
  let[@warning "-8"] (Receipt.Ticket_transfer_receipt { operation }) =
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

let test_noop_operation_no_exceptions () =
  let nonce = Nonce.of_n N.one in
  let level = Level.of_n N.one in
  let noop = Operation.Signed.noop ~identity:bob ~nonce ~level in
  let (Operation.Signed.Signed_operation { initial; _ }) = noop in
  let payload = [ initial ] in

  let _, _, exn =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[]
      Protocol.initial
  in
  Alcotest.(check int)
    "noop operation should not produce exceptions" 0 (List.length exn)

(*
   TODO: I think it can be a source a bug
      What if a user submit a noop operations ?
       - The operation is added to the mempool
       - And will never be removed ?
       - If the memory pool is full of noop operations, is it still possible to submit operations
*)
let test_noop_operation_empty_receipts () =
  let nonce = Nonce.of_n N.one in
  let level = Level.of_n N.one in
  let noop = Operation.Signed.noop ~identity:bob ~nonce ~level in
  let (Operation.Signed.Signed_operation { initial; _ }) = noop in
  let payload = [ initial ] in

  let _, receipts, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[]
      Protocol.initial
  in
  Alcotest.(check int)
    "noop operation should not produce exceptions" 0 (List.length receipts)

let test_noop_operation_does_nothing () =
  let nonce = Nonce.of_n N.one in
  let level = Level.of_n N.one in
  let noop = Operation.Signed.noop ~identity:bob ~nonce ~level in
  let (Operation.Signed.Signed_operation { initial; _ }) = noop in
  let payload = [ initial ] in
  let (Protocol.Protocol
        { ledger = initial_ledger; vm_state = initial_vm_state; _ }) =
    Protocol.initial
  in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[]
      Protocol.initial
  in
  let (Protocol.Protocol { ledger = next_ledger; vm_state = next_vm_state; _ })
      =
    protocol
  in
  Alcotest.(check bool)
    "noop operation should not change the ledger" true
    (initial_ledger = next_ledger);
  Alcotest.(check bool)
    "noop operation should not change the vm state" true
    (initial_vm_state = next_vm_state)

(* Maybe those tests are redundant with Ledger related test *)
let ticket_id =
  let ticketer =
    Ticket_id.Tezos
      (Deku_tezos.Contract_hash.of_b58 "KT1Us9LZaG8F6cskmMg1hB2FPRwakWkegkPi"
      |> Option.get)
  in
  let data = Bytes.of_string "hello" in
  Ticket_id.make ticketer data

let transfer ~level ~sender ~receiver ~amount =
  let nonce = Nonce.of_n N.one in
  let transfer =
    Operation.Signed.ticket_transfer ~identity:sender ~nonce ~level ~receiver
      ~ticket_id ~amount
  in
  let (Operation.Signed.Signed_operation { initial; _ }) = transfer in
  initial

let test_ticket_transfer_undefined_ticket_exn () =
  let amount = Amount.of_n N.zero in
  let level = Level.of_n N.one in
  let payload = [ transfer ~level ~sender:bob ~receiver:alice_addr ~amount ] in
  let protocol = Protocol.initial in
  let _, _, exns =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  Alcotest.(check int)
    "transfering undefined token should returns exception" 1 (List.length exns)

let test_ticket_transfer_undefined_ticket () =
  let amount = Amount.of_n N.zero in
  let level = Level.of_n N.one in
  let payload = [ transfer ~level ~sender:bob ~receiver:alice_addr ~amount ] in
  let protocol = Protocol.initial in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger = next_ledger; _ }) = protocol in
  Alcotest.(check bool)
    "transfering undefined token should not affect the ledger" true
    (ledger = next_ledger)

let test_ticket_transfer_undefined_ticket_receipt () =
  let amount = Amount.of_n N.zero in
  let level = Level.of_n N.one in
  let payload = [ transfer ~level ~sender:bob ~receiver:alice_addr ~amount ] in
  let protocol = Protocol.initial in
  let _, receipts, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  Alcotest.(check int)
    "transfering undefined token should return a receipt" 1
    (List.length receipts)

let protocol_with_ledger ~ledger =
  let ledger = Ledger.yojson_of_t ledger in
  let assoc =
    match Protocol.yojson_of_t Protocol.initial with
    | `Variant (_, Some assoc) -> assoc
    | `List [ _; assoc ] -> assoc
    | _ -> failwith "yojson error"
  in
  let assoc =
    assoc |> Yojson.Safe.Util.to_assoc
    |> List.map (fun (key, value) ->
           match (key, value) with
           | "ledger", _ -> ("ledger", ledger)
           | key, value -> (key, value))
  in
  `List [ `String "Protocol"; `Assoc assoc ] |> Protocol.t_of_yojson

let test_ticket_transfer_zero_ticket () =
  let amount = Amount.of_n N.zero in
  let level = Level.of_n N.one in
  let payload = [ transfer ~level ~sender:bob ~receiver:alice_addr ~amount ] in
  let ledger =
    Ledger.initial |> Ledger.deposit bob_addr (Amount.of_n N.one) ticket_id
  in
  let initial_bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let protocol = protocol_with_ledger ~ledger in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  Alcotest.(check amount')
    "balance should not change" initial_bob_balance bob_balance

let test_ticket_transfer_to_self () =
  let level = Level.of_n N.one in
  let amount = Amount.of_n N.one in
  let payload = [ transfer ~level ~sender:bob ~receiver:bob_addr ~amount ] in
  let ledger =
    Ledger.initial |> Ledger.deposit bob_addr (Amount.of_n N.one) ticket_id
  in
  let initial_bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let protocol = protocol_with_ledger ~ledger in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  Alcotest.(check amount')
    "balance should not change" initial_bob_balance bob_balance

let test_ticket_transfer_too_many_tickets () =
  let level = Level.of_n N.one in
  let amount = Amount.of_n (N.of_z (Z.of_int 32) |> Option.get) in
  let payload = [ transfer ~level ~sender:bob ~receiver:alice_addr ~amount ] in
  let ledger =
    Ledger.initial |> Ledger.deposit bob_addr (Amount.of_n N.one) ticket_id
  in
  let initial_bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let initial_alice_balance = Ledger.balance alice_addr ticket_id ledger in
  let protocol = protocol_with_ledger ~ledger in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let alice_balance = Ledger.balance alice_addr ticket_id ledger in
  Alcotest.(check amount')
    "balance of bob should not change" initial_bob_balance bob_balance;
  Alcotest.(check amount')
    "balance of alice &should not change" initial_alice_balance alice_balance

let test_ticket_transfer_tickets () =
  let level = Level.zero in
  let amount = Amount.one in
  let payload = [ transfer ~level ~sender:bob ~receiver:alice_addr ~amount ] in
  let ledger =
    let two = Z.of_int 2 |> N.of_z |> Option.get |> Amount.of_n in
    Ledger.initial |> Ledger.deposit bob_addr two ticket_id
  in
  let initial_bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let initial_alice_balance = Ledger.balance alice_addr ticket_id ledger in
  let protocol = protocol_with_ledger ~ledger in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let alice_balance = Ledger.balance alice_addr ticket_id ledger in

  let exepected_bob_balance =
    Amount.(initial_bob_balance - amount) |> Option.get
  in
  let exepected_alice_balance = Amount.(initial_alice_balance + amount) in

  Alcotest.(check amount')
    "balance should have descreased by 1 for bob" exepected_bob_balance
    bob_balance;
  Alcotest.(check amount')
    "balance should have increased by 1 for alice" exepected_alice_balance
    alice_balance

let test_ticket_transfer_all_tickets () =
  let level = Level.zero in
  let amount = Z.of_int 2 |> N.of_z |> Option.get |> Amount.of_n in
  let payload = [ transfer ~level ~sender:bob ~receiver:alice_addr ~amount ] in
  let ledger = Ledger.initial |> Ledger.deposit bob_addr amount ticket_id in
  let protocol = protocol_with_ledger ~ledger in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let alice_balance = Ledger.balance alice_addr ticket_id ledger in
  Alcotest.(check amount') "balance should be 0 for bob" Amount.zero bob_balance;
  Alcotest.(check amount') "balance should be 2 for alice" amount alice_balance

(* Withdraw tests *)

let withdraw ~level ~sender ~tezos_owner ~amount =
  let nonce = Nonce.of_n N.one in
  let transfer =
    Operation.Signed.withdraw ~identity:sender ~nonce ~level ~tezos_owner
      ~ticket_id ~amount
  in
  let (Operation.Signed.Signed_operation { initial; _ }) = transfer in
  initial

let test_withdraw_undefined_ticket () =
  let level = Level.zero in
  let amount = Amount.one in
  let payload =
    [ withdraw ~level ~sender:bob ~tezos_owner:bob_tezos ~amount ]
  in
  let protocol = Protocol.initial in
  let _, _, exns =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  Alcotest.(check int) "exception should be raised" 1 (List.length exns)

let test_withdraw_zero_ticket () =
  let level = Level.zero in
  let amount = Amount.zero in
  let payload =
    [ withdraw ~level ~sender:bob ~tezos_owner:bob_tezos ~amount ]
  in
  let ledger = Ledger.initial |> Ledger.deposit bob_addr amount ticket_id in
  let protocol = protocol_with_ledger ~ledger in
  let _, _, exns =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  Alcotest.(check int) "exception should be raised" 1 (List.length exns)

let test_withdraw_too_many_tickets () =
  let level = Level.zero in
  let amount = 32 |> Z.of_int |> N.of_z |> Option.get |> Amount.of_n in
  let payload =
    [ withdraw ~level ~sender:bob ~tezos_owner:bob_tezos ~amount ]
  in
  let ledger = Ledger.initial |> Ledger.deposit bob_addr Amount.one ticket_id in
  let protocol = protocol_with_ledger ~ledger in
  let _, _, exns =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  Alcotest.(check int) "exception should be raised" 1 (List.length exns)

let test_withdraw_all_tickets () =
  let level = Level.zero in
  let amount = 32 |> Z.of_int |> N.of_z |> Option.get |> Amount.of_n in
  let payload =
    [ withdraw ~level ~sender:bob ~tezos_owner:bob_tezos ~amount ]
  in
  let ledger = Ledger.initial |> Ledger.deposit bob_addr amount ticket_id in
  let protocol = protocol_with_ledger ~ledger in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  Alcotest.(check amount') "bob should have 0 tickets" Amount.zero bob_balance

let test_withdraw_one_ticket () =
  let level = Level.zero in
  let amount = Amount.one in
  let payload =
    [ withdraw ~level ~sender:bob ~tezos_owner:bob_tezos ~amount ]
  in
  let ledger =
    let amount = 32 |> Z.of_int |> N.of_z |> Option.get |> Amount.of_n in
    Ledger.initial |> Ledger.deposit bob_addr amount ticket_id
  in
  let bob_initial_balance = Ledger.balance bob_addr ticket_id ledger in
  let protocol = protocol_with_ledger ~ledger in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload ~tezos_operations:[] protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  let exepected_bob_balance =
    Amount.(bob_initial_balance - amount) |> Option.get
  in
  Alcotest.(check amount')
    "bob should have 0 tickets" exepected_bob_balance bob_balance

(* Deposit tests *)
let deposit ~destination ~amount =
  let destination = Address.to_key_hash destination |> Option.get in
  let operation =
    Tezos_operation.Deposit
      {
        destination;
        amount;
        ticket = Ticket_id.to_tezos_ticket ticket_id |> Option.get;
      }
  in
  let hash =
    Deku_tezos.Tezos_operation_hash.of_b58
      "ootmGcYJUiuuBir5MLnxKBYiGcdrHZoBp413obKQgUw6fNcLXd5"
    |> Option.get
  in
  Tezos_operation.{ hash; operations = [ operation ] }

let test_deposit_zero_ticket () =
  let level = Level.zero in
  let amount = Amount.zero in
  let tezos_operations = [ deposit ~destination:bob_addr ~amount ] in
  let protocol = Protocol.initial in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload:[] ~tezos_operations protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  Alcotest.(check amount') "balance should not change" Amount.zero bob_balance

let test_deposit_tickets () =
  let level = Level.zero in
  let amount = Amount.one in
  let tezos_operations = [ deposit ~destination:bob_addr ~amount ] in
  let protocol = Protocol.initial in
  let protocol, _, _ =
    Protocol.apply ~current_level:level ~payload:[] ~tezos_operations protocol
  in
  let (Protocol.Protocol { ledger; _ }) = protocol in
  let bob_balance = Ledger.balance bob_addr ticket_id ledger in
  Alcotest.(check amount') "balance should be updated" amount bob_balance

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
      ( "noop operation",
        [
          test_case "noop operation should not create exceptions" `Quick
            test_noop_operation_no_exceptions;
          test_case "noop operation should not create receipts" `Quick
            test_noop_operation_empty_receipts;
          test_case "noop operation should not update the protocol" `Quick
            test_noop_operation_does_nothing;
        ] );
      ( "ticket transfer",
        [
          test_case "cannot transfer undefined ticket" `Quick
            test_ticket_transfer_undefined_ticket_exn;
          test_case "undefined ticket transfer does not change the ledger"
            `Quick test_ticket_transfer_undefined_ticket;
          test_case "undefined ticket transfer returns a receipt " `Quick
            test_ticket_transfer_undefined_ticket_receipt;
          test_case "transfering 0 tickets have the same behaviour" `Quick
            test_ticket_transfer_zero_ticket;
          test_case "transfering tickets to self should change nothing" `Quick
            test_ticket_transfer_to_self;
          test_case
            "transfering tickets too many tickets should not change the balance"
            `Quick test_ticket_transfer_too_many_tickets;
          test_case
            "transfer ticket should decrease properly the balance of the \
             sender, and increase the balance of the receiver"
            `Quick test_ticket_transfer_tickets;
          test_case
            "After transfering all his tickets bob should have 0 tickets" `Quick
            test_ticket_transfer_all_tickets;
        ] );
      ( "ticket withdraw",
        [
          test_case "cannot withdraw undefined tickets" `Quick
            test_withdraw_undefined_ticket;
          test_case "cannot withdraw zero ticket" `Quick
            test_withdraw_zero_ticket;
          test_case "cannot withdraw more than balance" `Quick
            test_withdraw_too_many_tickets;
          test_case "can withdraw all tickets" `Quick test_withdraw_all_tickets;
          test_case "can withdraw only one ticket" `Quick
            test_withdraw_one_ticket;
        ] );
      ( "ticket deposit",
        [
          test_case "deposits 0 tickets does not affect the ledger" `Quick
            test_deposit_zero_ticket;
          test_case "deposits tickets update correctly the balance" `Quick
            test_deposit_tickets;
        ] );
    ]
