open Deku_crypto
open Deku_concepts

type operation_data =
  | Operation_transaction of { receiver : Address.t; amount : Amount.t }

and operation =
  | Operation of {
      (* TODO: Operation_hash.t *)
      hash : Operation_hash.t;
      level : Level.t;
      nonce : Nonce.t;
      source : Address.t;
      data : operation_data;
    }

and t = operation

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
  [@@deriving yojson]

  (* TODO: we could avoid Yojson.Safe.to_string if we had locations *)

  let hash operation =
    let json = yojson_of_operation operation in
    let json = Yojson.Safe.to_string json in
    Operation_hash.hash json

  let t_of_yojson json =
    let { level; nonce; source; data } = operation_of_yojson json in
    let data =
      match data with
      | Transaction { receiver; amount } ->
          Operation_transaction { receiver; amount }
    in
    let hash =
      let serialized = Yojson.Safe.to_string json in
      Operation_hash.hash serialized
    in
    Operation { hash; level; nonce; source; data }

  let yojson_of_t operation =
    let (Operation { hash = _; level; nonce; source; data }) = operation in
    let data =
      match data with
      | Operation_transaction { receiver; amount } ->
          Transaction { receiver; amount }
    in
    yojson_of_operation { level; nonce; source; data }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let transaction ~level ~nonce ~source ~receiver ~amount =
  let hash =
    let open Repr in
    let data = Transaction { receiver; amount } in
    let operation = { level; nonce; source; data } in
    hash operation
  in
  let data = Operation_transaction { receiver; amount } in
  Operation { hash; level; nonce; source; data }

let verify key signature operation =
  let (Operation { hash; source; _ }) = operation in
  match Key_hash.(equal (Address.to_key_hash source) (of_key key)) with
  | true ->
      let hash = Operation_hash.to_blake2b hash in
      Signature.verify key signature hash
  | false -> false
