open Deku_stdlib
open Deku_crypto
open Deku_concepts

exception Invalid_signature

type operation_content =
  | Operation_transaction of { receiver : Address.t; amount : Amount.t }

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
    | Transaction of { receiver : Address.t; amount : Amount.t }

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
      | Transaction { receiver; amount } ->
          Operation_transaction { receiver; amount }
    in
    (* TODO: serializing after deserializing *)
    let hash = hash operation in

    (match
       let source = Address.to_key_hash source in
       Key_hash.(equal source (of_key key))
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
      | Operation_transaction { receiver; amount } ->
          Transaction { receiver; amount }
    in
    let operation = { level; nonce; source; content } in
    yojson_of_operation_with_signature { key; signature; operation }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let transaction ~identity ~level ~nonce ~receiver ~amount =
  let source = Address.of_key_hash (Identity.key_hash identity) in
  let hash =
    let open Repr in
    let content = Transaction { receiver; amount } in
    let operation = { level; nonce; source; content } in
    hash operation
  in
  let key = Identity.key identity in
  let signature =
    let hash = Operation_hash.to_blake2b hash in
    Identity.sign ~hash identity
  in
  let content = Operation_transaction { receiver; amount } in
  Operation { key; signature; hash; level; nonce; source; content }

let is_in_includable_window ~current_level ~operation_level =
  let open Level in
  let open Deku_constants in
  let last_includable_block =
    let operation_level = to_n operation_level in
    of_n N.(operation_level + includable_operation_window)
  in
  (* limits for how many blocks we need to hold the operations *)
  last_includable_block > current_level
