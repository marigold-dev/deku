open Deku_crypto
open Deku_concepts
open Deku_protocol

type block =
  | Block of {
      signature : Signature.t;
      hash : Block_hash.t;
      author : Key.t;
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

exception Invalid_signature

module Repr = struct
  type block = {
    author : Key.t;
    level : Level.t;
    previous : Block_hash.t;
    payload : (Key.t * Signature.t * Operation.t) list;
  }

  and block_and_signature = { block : block; signature : Signature.t }
  [@@deriving yojson]

  (* TODO: we could avoid Yojson.Safe.to_string if we had locations *)
  let hash block =
    let json = yojson_of_block block in
    let json = Yojson.Safe.to_string json in
    Block_hash.hash json

  let t_of_yojson json =
    let { block; signature } = block_and_signature_of_yojson json in
    let { author; level; previous; payload } = block in
    let hash =
      let serialized = Yojson.Safe.to_string json in
      Block_hash.hash serialized
    in
    (match Signature.verify author signature (Block_hash.to_blake2b hash) with
    | true -> ()
    | false -> raise Invalid_signature);
    Block { signature; hash; author; level; previous; payload }

  let yojson_of_t block =
    let (Block { signature; hash = _; author; level; previous; payload }) =
      block
    in
    let block = { author; level; previous; payload } in
    yojson_of_block_and_signature { block; signature }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let produce ~identity ~level ~previous ~payload =
  let author = Identity.key identity in
  let hash = Repr.hash { author; level; previous; payload } in
  let signature =
    let hash = Block_hash.to_blake2b hash in
    let signature = Identity.sign ~hash identity in
    Verified_signature.signature signature
  in
  Block { signature; hash; author; level; previous; payload }

let sign ~identity block =
  let (Block { hash; _ }) = block in
  let hash = Block_hash.to_blake2b hash in
  Identity.sign ~hash identity
