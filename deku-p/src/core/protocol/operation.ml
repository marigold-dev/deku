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
  | Operation_attest_twitch_handle of {
      sender : Address.t;
      twitch_handle : string;
    }
  | Operation_attest_deku_address of {
      sender : Address.t;
      deku_address : Address.t;
      twitch_handle : Game.Twitch_handle.t;
    }
  | Operation_vote of { sender : Address.t; vote : Game.Vote.t }
  | Operation_delegated_vote of {
      sender : Address.t;
      twitch_handle : Game.Twitch_handle.t;
      vote : Game.Vote.t;
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
      case ~title:"attest_twitch_handle" (Tag 1)
        (obj3
           (req "type" (constant "attest_twitch_handle"))
           (req "sender" (Data_encoding.dynamic_size Address.encoding))
           (req "twitch_handle" string))
        (fun operation ->
          match operation with
          | Operation_attest_twitch_handle { sender; twitch_handle } ->
              Some ((), sender, twitch_handle)
          | _ -> None)
        (fun ((), sender, twitch_handle) ->
          Operation_attest_twitch_handle { sender; twitch_handle });
      case ~title:"attest_deku_address" (Tag 2)
        (obj4
           (req "type" (constant "attest_deku_address"))
           (req "sender" (Data_encoding.dynamic_size Address.encoding))
           (req "deku_address" (Data_encoding.dynamic_size Address.encoding))
           (req "twitch_handle" string))
        (fun operation ->
          match operation with
          | Operation_attest_deku_address
              { sender; deku_address; twitch_handle } ->
              Some ((), sender, deku_address, twitch_handle)
          | _ -> None)
        (fun ((), sender, deku_address, twitch_handle) ->
          Operation_attest_deku_address { sender; deku_address; twitch_handle });
      case ~title:"vote" (Tag 3)
        (obj3
           (req "type" (constant "vote"))
           (req "sender" (Data_encoding.dynamic_size Address.encoding))
           (req "vote" Game.Vote.encoding))
        (fun operation ->
          match operation with
          | Operation_vote { sender; vote } -> Some ((), sender, vote)
          | _ -> None)
        (fun ((), sender, vote) -> Operation_vote { sender; vote });
      case ~title:"delegated_vote" (Tag 4)
        (obj4
           (req "type" (constant "delegated_vote"))
           (req "sender" (Data_encoding.dynamic_size Address.encoding))
           (req "twitch_handle" string)
           (req "vote" Game.Vote.encoding))
        (fun operation ->
          match operation with
          | Operation_delegated_vote { sender; twitch_handle; vote } ->
              Some ((), sender, twitch_handle, vote)
          | _ -> None)
        (fun ((), sender, twitch_handle, vote) ->
          Operation_delegated_vote { sender; twitch_handle; vote });
      case ~title:"withdraw" (Tag 5)
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
      case ~title:"noop" (Tag 6)
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
    (Operation_attest_twitch_handle
       { sender = address; twitch_handle = "d4hines" });
  show_op
    (Operation_attest_deku_address
       { sender = address; deku_address = address; twitch_handle = "d4hines" });
  show_op
    (Operation_vote
       { sender = address; vote = Game.Vote.Input Deku_gameboy.Joypad.A });
  show_op
    (Operation_vote
       { sender = address; vote = Game.Vote.Governance Game.Anarchy });
  show_op
    (Operation_delegated_vote
       {
         sender = address;
         vote = Game.Vote.Input Deku_gameboy.Joypad.A;
         twitch_handle = "d4hines";
       });
  show_op
  @@ Operation_ticket_transfer
       { sender = address; receiver = address; ticket_id; amount = Amount.zero };
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
      { "type": "attest_twitch_handle",
        "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "twitch_handle": "d4hines" }
      ---------
      { "type": "attest_deku_address",
        "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "deku_address": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "twitch_handle": "d4hines" }
      ---------
      { "type": "vote", "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "vote": "A" }
      ---------
      { "type": "vote", "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "vote": "Anarchy" }
      ---------
      { "type": "delegated_vote", "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "twitch_handle": "d4hines", "vote": "A" }
      ---------
      { "type": "ticket_transfer",
        "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "receiver": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
        "ticket_id": [ "KT1LiabSxPyVUmVZCqHneCFLJrqQcLHkmX9d", "68656c6c6f" ],
        "amount": "0" }
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

  type hash_repr = Nonce.t * Level.t * operation

  let hash_encoding : hash_repr Data_encoding.t =
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

  let%expect_test "Hashing a Input Vote" =
    let sender =
      let secret =
        Secret.Ed25519
          (Ed25519.Secret.of_b58
             "edsk3MVrH9TbnFw7VsbBZX2yMNv4ApszZecLpPqrigx3HNnsDwQGio"
          |> Option.get)
      in
      Identity.make secret |> Identity.key_hash |> Address.of_key_hash
    in
    let open Deku_gameboy in
    let nonce = Nonce.of_n N.one in
    let level = Level.zero in
    let operation =
      Operation_attest_twitch_handle { sender; twitch_handle = "1337gmr" }
    in
    let operation_hash = hash ~nonce ~level ~operation in
    Format.printf "Operation_attest_twitch_handle: %a\n" Operation_hash.pp
      operation_hash;
    let operation =
      Operation_attest_deku_address
        { sender; deku_address = sender; twitch_handle = "1337gmr" }
    in
    let operation_hash = hash ~nonce ~level ~operation in
    Format.printf "Operation_attest_deku_address: %a\n" Operation_hash.pp
      operation_hash;
    let operation = Operation_vote { sender; vote = Input Joypad.A } in
    let operation_hash = hash ~nonce ~level ~operation in
    Format.printf "Operation_vote: %a\n" Operation_hash.pp operation_hash;
    let operation =
      Operation_delegated_vote
        { sender; vote = Input Joypad.A; twitch_handle = "1337gmr" }
    in
    let operation_hash = hash ~nonce ~level ~operation in
    Format.printf "Operation_delegated_vote: %a\n" Operation_hash.pp
      operation_hash;

    [%expect
      {|
      Operation_attest_twitch_handle: Do3nPqZQPXSVq7JmtWmus5WTBYf8PcLzZqgcr9wrL34Z1absaW2r
      Operation_attest_deku_address: Do41eSeJG6d5bAST6TWeESGuivoz8Qm3A6eRaTM5qMUvtHxAiKMN
      Operation_vote: Do3bzgbxFUWpkKp65otHmNqGRDXEjUHHQVc9y3CPHkeNgHHcpDWb
      Operation_delegated_vote: Do2w6q7qGjP4RjSYhcggYh6CRJwcLCFWJthvQcBWi5jwZNhvHSou |}]

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
    let show_initial initial =
      print_endline "-------";
      Format.printf "Pretty: %a\n%!" pp initial;
      let hex =
        Data_encoding.make_lazy encoding initial
        |> Data_encoding.force_bytes |> Hex.of_bytes |> Hex.show
      in
      Format.printf "Hex: %s\n%!" hex;
      let json = Data_encoding.Json.construct encoding initial in
      Format.printf "Json: %a\n%!" Data_encoding.Json.pp json
    in
    let nonce = Nonce.of_n N.zero in
    let sender =
      Address.of_b58 "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" |> Option.get
    in
    let operation = Operation_noop { sender } in
    let noop = make ~nonce ~level:Level.zero ~operation in
    show_initial noop;
    let operation =
      Operation_delegated_vote
        {
          sender;
          twitch_handle = "d4hines";
          vote = Game.Vote.Governance Game.Anarchy;
        }
    in
    let vote_operation = make ~nonce ~level:Level.zero ~operation in
    show_initial vote_operation;
    [%expect
      {|
      -------
      Pretty: Operation.Initial.Initial_operation {
                hash = Do3StPYpof6NyxvDj4GJhv6FXL84qHx9KWn6DJpJSQVC4M13QX5o;
                nonce = 0; level = 0;
                operation =
                Operation.Operation_noop {
                  sender = (Address.Implicit tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE)}}
      Hex: 000000010000000001000600005d9ac49706a3566b65f1ad56dd1433e4569a0367
      Json: { "nonce": "0", "level": 0,
              "operation":
                { "type": "noop",
                  "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE" } }
      -------
      Pretty: Operation.Initial.Initial_operation {
                hash = Do2nsjgRhfUYDJqNELzQSXyhqY11i78j1SSZwQn4CFY4Zw6hoyUQ;
                nonce = 0; level = 0;
                operation =
                Operation.Operation_delegated_vote {
                  sender = (Address.Implicit tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE);
                  twitch_handle = "d4hines";
                  vote = (Game.Vote.Governance Game.Anarchy)}}
      Hex: 00000001000000000100040000001600005d9ac49706a3566b65f1ad56dd1433e4569a036700000007643468696e6573000000000100
      Json: { "nonce": "0", "level": 0,
              "operation":
                { "type": "delegated_vote",
                  "sender": "tz1UAxwRXXDvpZ5sAanbbP8tjKBoa2dxKUHE",
                  "twitch_handle": "d4hines", "vote": "Anarchy" } } |}]

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
          | Operation_attest_twitch_handle { sender; _ } -> sender
          | Operation_attest_deku_address { sender; _ } -> sender
          | Operation_vote { sender; _ } -> sender
          | Operation_delegated_vote { sender; _ } -> sender
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

  let vote ~nonce ~level ~vote ~identity =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation = Operation_vote { sender; vote } in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial

  let delegated_vote ~nonce ~level ~vote ~twitch_handle ~identity =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation = Operation_delegated_vote { sender; twitch_handle; vote } in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial

  let attest_twitch_handle ~nonce ~level ~twitch_handle ~identity =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation = Operation_attest_twitch_handle { sender; twitch_handle } in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial

  let attest_deku_address ~nonce ~level ~deku_address ~twitch_handle ~identity =
    let sender = Address.of_key_hash (Identity.key_hash identity) in
    let operation =
      Operation_attest_deku_address { sender; deku_address; twitch_handle }
    in
    let initial = Initial.make ~nonce ~level ~operation in
    make ~identity ~initial
end
