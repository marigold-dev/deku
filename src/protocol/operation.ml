open Deku_crypto
open Deku_concepts

exception Invalid_signature

type operation_data =
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
      data : operation_data;
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

module Set = Set.Make (struct
  type t = operation

  let compare = compare
end)

module Repr = struct
  type operation_data =
    | Transaction of { receiver : Address.t; amount : Amount.t }

  and operation = {
    level : Level.t;
    nonce : Nonce.t;
    source : Address.t;
    data : operation_data;
  }

  and operation_with_signature = {
    key : Key.t;
    signature : Signature.t;
    operation : operation;
  }
  [@@deriving yojson]

  (* TODO: we could avoid Yojson.Safe.to_string if we had locations *)

  let hash operation =
    let json = yojson_of_operation operation in
    let json = Yojson.Safe.to_string json in
    Operation_hash.hash json

  let t_of_yojson json =
    let { key; signature; operation } =
      operation_with_signature_of_yojson json
    in
    let { level; nonce; source; data } = operation in
    let data =
      match data with
      | Transaction { receiver; amount } ->
          Operation_transaction { receiver; amount }
    in
    let hash =
      let serialized = Yojson.Safe.to_string json in
      Operation_hash.hash serialized
    in

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
    Operation { key; signature; hash; level; nonce; source; data }

  let yojson_of_t operation =
    let (Operation { key; signature; hash = _; level; nonce; source; data }) =
      operation
    in
    let data =
      match data with
      | Operation_transaction { receiver; amount } ->
          Transaction { receiver; amount }
    in
    let operation = { level; nonce; source; data } in
    yojson_of_operation_with_signature { key; signature; operation }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let transaction ~identity ~level ~nonce ~source ~receiver ~amount =
  let hash =
    let open Repr in
    let data = Transaction { receiver; amount } in
    let operation = { level; nonce; source; data } in
    hash operation
  in
  let key = Identity.key identity in
  let signature =
    let hash = Operation_hash.to_blake2b hash in
    Identity.sign ~hash identity
  in
  let data = Operation_transaction { receiver; amount } in
  Operation { key; signature; hash; level; nonce; source; data }
