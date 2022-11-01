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
      operation : Ocaml_wasm_vm.Operation.t;
    }
  | Operation_withdraw of {
      sender : Address.t;
      owner : Deku_tezos.Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }
  | Operation_noop of { sender : Address.t }

and t = operation [@@deriving show, yojson]

let encoding =
  (* TODO: bench Data_encoding.union vs Data_encoding.matching*)
  let open Data_encoding in
  union ~tag_size:`Uint8
    [
      case ~title:"ticket_transfer" (Tag 0)
        (Data_encoding.dynamic_size
           (tup4
              (Data_encoding.dynamic_size Address.encoding)
              (Data_encoding.dynamic_size Address.encoding)
              Ticket_id.encoding Amount.encoding))
        (fun operation ->
          match operation with
          | Operation_ticket_transfer { sender; receiver; ticket_id; amount } ->
              Some (sender, receiver, ticket_id, amount)
          | _ -> None)
        (fun (sender, receiver, ticket_id, amount) ->
          Operation_ticket_transfer { sender; receiver; ticket_id; amount });
      case ~title:"vm_transaction" (Tag 1)
        (tup2
           (Data_encoding.dynamic_size Address.encoding)
           Ocaml_wasm_vm.Operation.encoding)
        (fun operation ->
          match operation with
          | Operation_vm_transaction { sender; operation } ->
              Some (sender, operation)
          | _ -> None)
        (fun (sender, operation) ->
          Operation_vm_transaction { sender; operation });
      case ~title:"withdraw" (Tag 2)
        (tup4
           (Data_encoding.dynamic_size Address.encoding)
           Ticket_id.encoding Amount.encoding Deku_tezos.Address.encoding)
        (fun operation ->
          match operation with
          | Operation_withdraw { sender; owner; ticket_id; amount } ->
              Some (sender, ticket_id, amount, owner)
          | _ -> None)
        (fun (sender, ticket_id, amount, owner) ->
          Operation_withdraw { sender; owner; ticket_id; amount });
      case ~title:"noop" (Tag 3) Address.encoding
        (fun operation ->
          match operation with
          | Operation_noop { sender } -> Some sender
          | _ -> None)
        (fun sender -> Operation_noop { sender });
    ]

module Initial = struct
  type initial_operation =
    | Initial_operation of {
        hash : Operation_hash.t;
        nonce : Nonce.t;
        level : Level.t;
        operation : operation;
      }

  and t = initial_operation [@@deriving show, yojson]

  let hash_encoding = Data_encoding.tup3 Nonce.encoding Level.encoding encoding

  let hash ~nonce ~level ~operation =
    let binary =
      Data_encoding.Binary.to_string_exn hash_encoding (nonce, level, operation)
    in
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

  let t_of_yojson json =
    let json = Yojson.Safe.to_string json in
    let json = Result.get_ok (Data_encoding.Json.from_string json) in
    Data_encoding.Json.destruct encoding json

  let yojson_of_t signed =
    let json = Data_encoding.Json.construct encoding signed in
    let json = Data_encoding.Json.to_string json in
    Yojson.Safe.from_string json

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
