open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol

type block =
  | Block of {
      key : Key.t;
      signature : Signature.t;
      hash : Block_hash.t;
      author : Key_hash.t;
      level : Level.t;
      (* TODO: nonce *)
      previous : Block_hash.t;
      payload : string list;
    }

type t = block

let equal a b =
  let (Block { hash = a; _ }) = a in
  let (Block { hash = b; _ }) = b in
  Block_hash.equal a b

let compare a b =
  let (Block { hash = a; _ }) = a in
  let (Block { hash = b; _ }) = b in
  Block_hash.compare a b

exception Invalid_signature

module Repr = struct
  type block_and_signature = {
    key : Key.t;
    signature : Signature.t;
    block : block;
  }

  and block = {
    author : Key_hash.t;
    level : Level.t;
    previous : Block_hash.t;
    payload : string list;
  }
  [@@deriving yojson]

  (* TODO: we could avoid Yojson.Safe.to_string if we had locations *)
  let hash block =
    let json = yojson_of_block block in
    let json = Yojson.Safe.to_string json in
    Block_hash.hash json

  let t_of_yojson json =
    let { key; signature; block } = block_and_signature_of_yojson json in
    let { author; level; previous; payload } = block in
    (* TODO: serializing after deserializing *)
    let hash = hash block in

    (match Key_hash.(equal author (of_key key)) with
    | true -> ()
    | false -> raise Invalid_signature);
    (match
       let hash = Block_hash.to_blake2b hash in
       Signature.verify key signature hash
     with
    | true -> ()
    | false -> raise Invalid_signature);
    Block { key; signature; hash; author; level; previous; payload }

  let yojson_of_t block =
    let (Block { key; signature; hash = _; author; level; previous; payload }) =
      block
    in
    let block = { author; level; previous; payload } in
    yojson_of_block_and_signature { key; signature; block }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let payload_of_operations operations =
  (* TODO: produce parallel? *)
  (* TODO: this likely should not be here *)
  List.map
    (fun operation ->
      let json = Operation.yojson_of_t operation in
      Yojson.Safe.to_string json)
    operations

let produce ~identity ~level ~previous ~operations =
  let author = Identity.key_hash identity in
  let payload = payload_of_operations operations in
  let hash = Repr.hash { author; level; previous; payload } in
  let key = Identity.key identity in
  let signature =
    let hash = Block_hash.to_blake2b hash in
    Identity.sign ~hash identity
  in
  Block { key; signature; hash; author; level; previous; payload }

let sign ~identity block =
  let (Block { hash; _ }) = block in
  let hash = Block_hash.to_blake2b hash in
  Verified_signature.sign hash identity

module Set = Set.Make (struct
  type t = block

  let compare = compare
  let t_of_yojson = t_of_yojson
  let yojson_of_t = yojson_of_t
end)
