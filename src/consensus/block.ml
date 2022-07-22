open Deku_crypto
open Deku_concepts
open Deku_protocol

type block =
  | Block of {
      hash : Block_hash.t;
      author : Key_hash.t;
      level : Level.t;
      previous : Block_hash.t;
      payload : (Key.t * Signature.t * Operation.t) list;
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

module Set = Set.Make (struct
  type t = block

  let compare = compare
end)

module Repr = struct
  type block = {
    author : Key_hash.t;
    level : Level.t;
    previous : Block_hash.t;
    payload : (Key.t * Signature.t * Operation.t) list;
  }
  [@@deriving yojson]

  (* TODO: we could avoid Yojson.Safe.to_string if we had locations *)
  let hash block =
    let json = yojson_of_block block in
    let json = Yojson.Safe.to_string json in
    Block_hash.hash json

  let t_of_yojson json =
    let { author; level; previous; payload } = block_of_yojson json in
    let hash =
      let serialized = Yojson.Safe.to_string json in
      Block_hash.hash serialized
    in
    Block { hash; author; level; previous; payload }

  let yojson_of_t block =
    let (Block { hash = _; author; level; previous; payload }) = block in
    yojson_of_block { author; level; previous; payload }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let make ~author ~level ~previous ~payload =
  let hash = Repr.hash { author; level; previous; payload } in
  Block { hash; author; level; previous; payload }

let sign ~identity block =
  let (Block { hash; _ }) = block in
  let hash = Block_hash.to_blake2b hash in
  Identity.sign ~hash identity
