open Deku_crypto
open Deku_concepts

exception Invalid_signature

type operation_content =
  | Operation_transaction of {
      receiver : Address.t;
      ticket_id : Ticket_id.t;
      amount : Amount.t;
    }

and operation =
  | Operation of {
      (* TODO: I don't like that operation carries signature *)
      key : Key.t;
      signature : Signature.t;
      hash : Operation_hash.t;
      level : Level.t;
      nonce : Nonce.t;
      source : Address.t;
      content : operation_content;
    }

and t = operation [@@deriving yojson]

let equal a b =
  let (Operation { hash = a; _ }) = a in
  let (Operation { hash = b; _ }) = b in
  Operation_hash.equal a b

let compare a b =
  let (Operation { hash = a; _ }) = a in
  let (Operation { hash = b; _ }) = b in
  Operation_hash.compare a b

module Repr = struct
  type operation_content =
    | Transaction of {
        receiver : Address.t;
        ticket_id : Ticket_id.ticket_id;
        amount : Amount.t;
      }

  and operation = {
    level : Level.t;
    nonce : Nonce.t;
    source : Address.t;
    content : operation_content;
  }

  and operation_with_signature = {
    key : Key.t;
    signature : Signature.t;
    operation : operation;
  }
  [@@deriving yojson]

  let hash operation =
    let json = yojson_of_operation operation in
    let json = Yojson.Safe.to_string json in
    Operation_hash.hash json

  let t_of_yojson json =
    let { key; signature; operation } =
      operation_with_signature_of_yojson json
    in
    let { level; nonce; source; content } = operation in
    let content =
      match content with
      | Transaction { receiver; ticket_id; amount } ->
          Operation_transaction { receiver; ticket_id; amount }
    in
    (* TODO: serializing after deserializing *)
    let hash = hash operation in

    (match
       let key_hash = key |> Key_hash.of_key |> Address.of_key_hash in
       Address.equal source key_hash
     with
    | true -> ()
    | false -> raise Invalid_signature);
    (match
       let hash = Operation_hash.to_blake2b hash in
       Signature.verify key signature hash
     with
    | true -> ()
    | false -> raise Invalid_signature);
    Operation { key; signature; hash; level; nonce; source; content }

  let yojson_of_t operation =
    let (Operation { key; signature; hash = _; level; nonce; source; content })
        =
      operation
    in
    let content =
      match content with
      | Operation_transaction { receiver; ticket_id; amount } ->
          Transaction { receiver; ticket_id; amount }
    in
    let operation = { level; nonce; source; content } in
    yojson_of_operation_with_signature { key; signature; operation }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let transaction ~identity ~level ~nonce ~source ~receiver ~ticket_id ~amount =
  let hash =
    let open Repr in
    let content = Transaction { receiver; ticket_id; amount } in
    let operation = { level; nonce; source; content } in
    hash operation
  in
  let key = Identity.key identity in
  let signature =
    let hash = Operation_hash.to_blake2b hash in
    Identity.sign ~hash identity
  in
  let content = Operation_transaction { receiver; ticket_id; amount } in
  Operation { key; signature; hash; level; nonce; source; content }
