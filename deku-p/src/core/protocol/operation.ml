open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_ledger

exception Invalid_signature
exception Invalid_source

type operation =
  | Operation_ticket_transfer of {
      sender : Address.t;
      receiver : Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Operation_vm_transaction of {
      sender : Address.t;
      operation : Ocaml_wasm_vm.Operation_payload.t;
    }
  | Operation_withdraw of {
      sender : Address.t;
      owner : Deku_tezos.Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Operation_noop of { sender : Address.t }

and t = operation [@@deriving show]

let encoding =
  (* TODO: bench Data_encoding.union vs Data_encoding.matching*)
  let open Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"ticket_transfer" (Tag 0)
        (Data_encoding.dynamic_size
           (obj5
              (req "type" (constant "ticket_transfer"))
              (req "sender" (Data_encoding.dynamic_size Address.encoding))
              (req "receiver" (Data_encoding.dynamic_size Address.encoding))
              (req "ticket_id" Ticket_id.encoding)
              (req "amount" Amount.encoding)))
        (fun operation ->
          match operation with
          | Operation_ticket_transfer { sender; receiver; ticket_id; amount } ->
              Some ((), sender, receiver, ticket_id, amount)
          | _ -> None)
        (fun ((), sender, receiver, ticket_id, amount) ->
          Operation_ticket_transfer { sender; receiver; ticket_id; amount });
      case ~title:"vm_transaction" (Tag 1)
        (obj3
           (req "type" (constant "vm_transaction"))
           (req "sender" (Data_encoding.dynamic_size Address.encoding))
           (req "operation" Ocaml_wasm_vm.Operation_payload.encoding))
        (fun operation ->
          match operation with
          | Operation_vm_transaction { sender; operation } ->
              Some ((), sender, operation)
          | _ -> None)
        (fun ((), sender, operation) ->
          Operation_vm_transaction { sender; operation });
      case ~title:"withdraw" (Tag 2)
        (obj5
           (req "type" (constant "withdraw"))
           (req "sender" (Data_encoding.dynamic_size Address.encoding))
           (req "ticket_id" Ticket_id.encoding)
           (req "amount" Amount.encoding)
           (req "owner" Deku_tezos.Address.encoding))
        (fun operation ->
          match operation with
          | Operation_withdraw { sender; owner; ticket_id; amount } ->
              Some ((), sender, ticket_id, amount, owner)
          | _ -> None)
        (fun ((), sender, ticket_id, amount, owner) ->
          Operation_withdraw { sender; owner; ticket_id; amount });
      case ~title:"noop" (Tag 3)
        (obj2 (req "type" (constant "noop")) (req "sender" Address.encoding))
        (fun operation ->
          match operation with
          | Operation_noop { sender } -> Some ((), sender)
          | _ -> None)
        (fun ((), sender) -> Operation_noop { sender });
    ]

let%expect_test "Operation encoding" =
  let address =
    Address.of_b58 "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" |> Option.get
  in
  let tezos_address =
    Deku_tezos.Address.of_string "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE"
    |> Option.get
  in
  let contract_address =
    Deku_tezos.Contract_hash.of_b58 "KT1LiabSxPyVUmVZCqHneCFLJrqQcLHkmX9d"
    |> Option.get
  in
  let ticketer = Ticket_id.Tezos contract_address in
  let ticket_id = Ticket_id.make ticketer (Bytes.of_string "hello") in
  let show_op op =
    let json = Data_encoding.Json.construct encoding op in
    Format.printf "%a\n---------\n%!" Data_encoding.Json.pp json
  in
  show_op
  @@ Operation_ticket_transfer
       { sender = address; receiver = address; ticket_id; amount = Amount.zero };

  let operation =
    let open Ocaml_wasm_vm in
    let argument = Value.(Union (Left (Union (Right (Int (Z.of_int 5)))))) in
    let operation = Operation.Call { address; argument } in
    Operation_payload.{ operation; tickets = [ (ticket_id, Amount.zero) ] }
  in
  (* TODO: this one is a big ugly with nested "operation" keys. We should fix it. *)
  show_op @@ Operation_vm_transaction { sender = address; operation };
  show_op
  @@ Operation_withdraw
       {
         sender = address;
         owner = tezos_address;
         ticket_id;
         amount = Amount.zero;
       };
  show_op @@ Operation_noop { sender = address };
  [%expect
    {|
      { "type": "ticket_transfer",
        "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "receiver": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "ticket_id": [ "KT1LiabSxPyVUmVZCqHneCFLJrqQcLHkmX9d", "68656c6c6f" ],
        "amount": "0" }
      ---------
      { "type": "vm_transaction", "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "operation":
          { "operation":
              { "address": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
                "argument":
                  [ "Union", [ "Left", [ "Union", [ "Right", [ "Int", "5" ] ] ] ] ] },
            "tickets":
              [ [ [ "KT1LiabSxPyVUmVZCqHneCFLJrqQcLHkmX9d", "68656c6c6f" ], "0" ] ] } }
      ---------
      { "type": "withdraw", "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "ticket_id": [ "KT1LiabSxPyVUmVZCqHneCFLJrqQcLHkmX9d", "68656c6c6f" ],
        "amount": "0", "owner": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" }
      ---------
      { "type": "noop", "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" }
      --------- |}]

module Initial = struct
  type initial_operation =
    | Initial_operation of {
        hash : Operation_hash.t;
        nonce : Nonce.t;
        level : Level.t;
        operation : operation;
      }

  and t = initial_operation [@@deriving show]

  let hash_encoding =
    let open Data_encoding in
    obj3
      (req "nonce" Nonce.encoding)
      (req "level" Level.encoding)
      (req "operation" encoding)

  let hash ~nonce ~level ~operation =
    let binary =
      Data_encoding.Binary.to_string_exn hash_encoding (nonce, level, operation)
    in
    let binary = "\x80" ^ binary in
    Operation_hash.hash binary

  let make ~nonce ~level ~operation =
    let hash = hash ~nonce ~level ~operation in
    Initial_operation { hash; nonce; level; operation }

  let encoding =
    let open Data_encoding in
    conv
      (fun (Initial_operation { hash = _; nonce; level; operation }) ->
        (nonce, level, operation))
      (fun (nonce, level, operation) ->
        let hash = hash ~nonce ~level ~operation in
        Initial_operation { hash; nonce; level; operation })
      hash_encoding

  let%expect_test "Initial encoding" =
    let nonce = Nonce.of_n N.zero in
    let sender =
      Address.of_b58 "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" |> Option.get
    in
    let operation = Operation_noop { sender } in
    let initial = make ~nonce ~level:Level.zero ~operation in
    Format.printf "Pretty: %a\n%!" pp initial;
    let hex =
      Data_encoding.make_lazy encoding initial
      |> Data_encoding.force_bytes |> Hex.of_bytes |> Hex.show
    in
    Format.printf "Hex: %s\n%!" hex;
    let json = Data_encoding.Json.construct encoding initial in
    Format.printf "Json: %a\n%!" Data_encoding.Json.pp json;
    [%expect
      {|
      Pretty: Operation.Initial.Initial_operation {
                hash = Do2XVsHk8txd6V4YTt6io1nyJMHujD6APbhWWW64DwM3y8D5XxhF;
                nonce = 0; level = 0;
                operation =
                Operation.Operation_noop {
                  sender = (Address.Implicit tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE)}}
      Hex: 000000010000000001000300005d9ac49706a3566b65f1ad56dd1433e4569a0367
      Json: { "nonce": "0", "level": 0,
              "operation":
                { "type": "noop",
                  "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" } } |}]

  let includable_operation_window = Deku_constants.includable_operation_window

  let last_includable_level operation =
    let (Initial_operation { level = operation_level; _ }) = operation in
    let operation_level = Level.to_n operation_level in
    Level.of_n N.(operation_level + includable_operation_window)

  (* TODO: This seems like a weird place to put this function *)
  let is_in_includable_window ~current_level ~operation_level =
    let last_includable_block =
      let operation_level = Level.to_n operation_level in
      Level.of_n N.(operation_level + includable_operation_window)
    in
    (* limits for how many blocks we need to hold the operations *)
    Level.(last_includable_block > current_level)
end

module Signed = struct
  type signed_operation =
    | Signed_operation of {
        key : Key.t;
        signature : Signature.t;
        initial : Initial.t;
      }

  and t = signed_operation [@@deriving show]

  let make ~identity ~initial =
    let key = Identity.key identity in
    let signature =
      let open Initial in
      let (Initial_operation { hash; _ }) = initial in
      let hash = Operation_hash.to_blake2b hash in
      Identity.sign ~hash identity
    in
    Signed_operation { key; signature; initial }

  let encoding =
    let open Data_encoding in
    conv_with_guard
      (fun (Signed_operation { key; signature; initial }) ->
        ((key, signature), initial))
      (fun ((key, signature), initial) ->
        let (Initial_operation { hash; nonce = _; level = _; operation }) =
          initial
        in
        let sender =
          match operation with
          | Operation_ticket_transfer { sender; _ } -> sender
          | Operation_vm_transaction { sender; _ } -> sender
          | Operation_withdraw { sender; _ } -> sender
          | Operation_noop { sender } -> sender
        in
        let sender = Address.to_key_hash sender in
        let hash = Operation_hash.to_blake2b hash in
        match
          Option.equal Key_hash.equal (Some (Key_hash.of_key key)) sender
          && Signature.verify key signature hash
        with
        | true -> Ok (Signed_operation { key; signature; initial })
        | false -> Error "Invalid operation signature")
      (tup2 Signature.key_encoding Initial.encoding)

  let make_with_signature ~key ~signature ~initial =
    let (Initial.Initial_operation { hash; _ }) = initial in
    match Signature.verify key signature (Operation_hash.to_blake2b hash) with
    | false -> None
    | true -> Some (Signed_operation { key; signature; initial })

  let ticket_transfer ~identity ~nonce ~level ~receiver ~ticket_id ~amount =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation =
      Operation_ticket_transfer { sender; receiver; ticket_id; amount }
    in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial

  let noop ~identity ~nonce ~level =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation = Operation_noop { sender } in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial

  let withdraw ~identity ~nonce ~level ~tezos_owner ~ticket_id ~amount =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation =
      Operation_withdraw { sender; owner = tezos_owner; ticket_id; amount }
    in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial

  let vm_transaction ~nonce ~level ~content ~identity =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation = Operation_vm_transaction { sender; operation = content } in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial
end
