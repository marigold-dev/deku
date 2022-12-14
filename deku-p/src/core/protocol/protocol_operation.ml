open Deku_repr
open Deku_stdlib
open Deku_crypto
open Deku_concepts

exception Invalid_signature
exception Invalid_source

module Internal = struct
  type internal_operation =
    (* TODO: ticket_book? Maybe split ledger and address_book? *)
    (* TODO: dependent register -> transfer?
        if we add owner to register it would be possible to
          make a contract to automate this *)
    | Internal_operation_register of {
        sender : Contract.t;
        ticket_id : Ticket_id.t;
      }
    | Internal_operation_transfer of {
        sender : Contract.t;
        sender_code : Ledger_code.t;
        receiver_code : Ledger_code.t;
        amount : Amount.t;
      }
    | Internal_operation_noop of { sender : Contract.t }

  and t = internal_operation [@@deriving show]

  let sender operation =
    match operation with
    | Internal_operation_register { sender; ticket_id = _ }
    | Internal_operation_transfer
        { sender; sender_code = _; receiver_code = _; amount = _ } ->
        sender
    | Internal_operation_noop { sender } -> sender

  let encoding =
    (* TODO: bench Data_encoding.union vs Data_encoding.matching*)
    let open Data_encoding in
    union ~tag_size:`Uint8
      [
        case ~title:"register" (Tag 0)
          (tup2 Contract.encoding Ticket_id.encoding)
          (fun operation ->
            match operation with
            | Internal_operation_register { sender; ticket_id } ->
                Some (sender, ticket_id)
            | _ -> None)
          (fun (sender, ticket_id) ->
            Internal_operation_register { sender; ticket_id });
        case ~title:"transfer" (Tag 1)
          (tup4 Contract.encoding Ledger_code.encoding Ledger_code.encoding
             Amount.encoding)
          (fun operation ->
            match operation with
            | Internal_operation_transfer
                { sender; sender_code; receiver_code; amount } ->
                Some (sender, sender_code, receiver_code, amount)
            | _ -> None)
          (fun (sender, sender_code, receiver_code, amount) ->
            Internal_operation_transfer
              { sender; sender_code; receiver_code; amount });
        case ~title:"noop" (Tag 2) Contract.encoding
          (fun operation ->
            match operation with
            | Internal_operation_noop { sender } -> Some sender
            | _ -> None)
          (fun sender -> Internal_operation_noop { sender });
      ]
end

module Initial = struct
  module Hash = struct
    type initial_operation_hash = BLAKE2b.t
    and t = initial_operation_hash [@@deriving show]

    let hash = BLAKE2b.hash
  end

  module Nonce = struct
    (* TODO: should we prefix level in b58? *)
    type nonce = N.t
    and t = nonce [@@deriving show]

    (* repr *)
    let of_n n = n
    let to_n nonce = nonce
    let encoding = N.encoding
  end

  type initial_operation =
    | Initial_operation of {
        hash : Hash.t;
        nonce : Nonce.t;
        level : Level.t;
        source : Key_hash.t;
        operation : Internal.t;
      }

  and t = initial_operation [@@deriving show]

  let hash_encoding =
    Data_encoding.tup4 Nonce.encoding Level.encoding Key_hash.encoding
      Internal.encoding

  let hash ~nonce ~level ~operation ~source =
    let binary =
      Data_encoding.Binary.to_string_exn hash_encoding
        (nonce, level, source, operation)
    in
    Hash.hash binary

  let make ~nonce ~level ~operation ~source =
    let hash = hash ~nonce ~level ~operation ~source in
    Initial_operation { hash; nonce; level; source; operation }

  let encoding =
    let open Data_encoding in
    conv_with_guard
      (fun (Initial_operation { hash = _; nonce; level; source; operation }) ->
        (nonce, level, source, operation))
      (fun (nonce, level, source, operation) ->
        let hash = hash ~nonce ~level ~operation ~source in
        let actual_sender = Internal.sender operation in
        let expected_sender = Contract.of_key_hash source in
        match Contract.equal actual_sender expected_sender with
        | true ->
            Ok (Initial_operation { hash; nonce; level; source; operation })
        | false -> Error "Invalid internal operation source")
      hash_encoding

  let includable_operation_window = Deku_constants.includable_operation_window

  let last_includable_level ~operation_level =
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
      Identity.sign ~hash identity
    in
    Signed_operation { key; signature; initial }

  let make_with_signature ~key ~signature ~initial =
    let (Initial.Initial_operation
          { hash; nonce = _; level = _; source; operation = _ }) =
      initial
    in
    match
      Key_hash.equal (Key_hash.of_key key) source
      && Signature.verify key signature hash
    with
    | true -> Some (Signed_operation { key; signature; initial })
    | false -> None

  let transfer ~identity ~nonce ~level ~sender_code ~receiver_code ~amount =
    let source = Identity.key_hash identity in
    let sender = Contract.of_key_hash source in
    let operation =
      Internal.Internal_operation_transfer
        { sender; sender_code; receiver_code; amount }
    in
    let initial = Initial.make ~nonce ~level ~source ~operation in
    make ~identity ~initial

  let noop ~identity ~nonce ~level =
    let source = Identity.key_hash identity in
    let sender = Contract.of_key_hash source in
    let operation = Internal.Internal_operation_noop { sender } in
    let initial = Initial.make ~nonce ~level ~source ~operation in
    make ~identity ~initial
end

module Raw = struct
  module Hash = struct
    open BLAKE2b

    type raw_operation_hash = BLAKE2b.t
    and t = raw_operation_hash [@@deriving show]

    include With_b58_and_encoding_and_yojson (struct
      let name = "Deku_protocol.Protocol_operation.Raw.Hash"
      let prefix = Prefix.deku_raw_operation_hash
    end)

    let hash = hash
  end

  type raw_operation =
    | Raw_operation of {
        hash : Hash.t;
        key : Key.t;
        signature : Signature.t;
        initial : Initial.t;
      }

  and t = raw_operation [@@deriving show]

  let hash_encoding =
    let open Data_encoding in
    (* TODO: remove all duplicated data from encodings
        initial has sender, which could be derived from the key *)
    tup2 Signature.key_encoding Initial.encoding

  let hash ~key ~signature ~initial =
    let binary =
      Data_encoding.Binary.to_string_exn hash_encoding
        ((key, signature), initial)
    in
    Hash.hash binary

  let of_signed signed =
    let open Signed in
    let (Signed_operation { key; signature; initial }) = signed in
    let hash = hash ~key ~signature ~initial in
    Raw_operation { hash; key; signature; initial }

  let encoding =
    let open Data_encoding in
    conv
      (fun (Raw_operation { hash = _; key; signature; initial }) ->
        ((key, signature), initial))
      (fun ((key, signature), initial) ->
        let hash = hash ~key ~signature ~initial in
        Raw_operation { hash; key; signature; initial })
      hash_encoding

  let hash raw =
    let (Raw_operation { hash; key = _; signature = _; initial = _ }) = raw in
    hash

  let level raw =
    let open Initial in
    let (Raw_operation { hash = _; key = _; signature = _; initial }) = raw in
    let (Initial_operation
          { hash = _; nonce = _; level; source = _; operation = _ }) =
      initial
    in
    level

  let verify raw =
    let (Raw_operation { hash = _; key; signature; initial }) = raw in
    Signed.make_with_signature ~key ~signature ~initial
end
