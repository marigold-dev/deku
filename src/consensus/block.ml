open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol

type block =
  | Block of {
      key : Key.t;
      signature : Signature.t;
      hash : Block_hash.t;
      (* FIXME: withdrawal_handles_hash??? TYPO*)
      withdrawal_handles_hash : BLAKE2b.t;
      author : Key_hash.t;
      level : Level.t;
      (* TODO: nonce *)
      previous : Block_hash.t;
      payload : string list;
      payload_hash : BLAKE2b.t;
      tezos_operations : Tezos_operation.t list;
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
    withdrawal_handles_hash : Ledger.Withdrawal_handle.hash;
    payload : string list;
    tezos_operations : Tezos_operation.t list;
  }
  [@@deriving yojson]

  (* TODO: bad naming*)
  module Payload_hash = struct
    type payload_hash = {
      author : Key_hash.t;
      level : Level.t;
      previous : Block_hash.t;
      withdrawal_handles_hash : Ledger.Withdrawal_handle.hash;
      payload : BLAKE2b.t;
      tezos_operations : Tezos_operation.t list;
    }
    [@@deriving yojson_of]

    let hash ~author ~level ~previous ~withdrawal_handles_hash ~payload
        ~tezos_operations =
      let payload_size = List.length payload in
      let chunk_size =
        max (payload_size / Deku_constants.max_payload_chunks) 1
      in
      let chunks = List.chunks_of ~length:chunk_size payload in
      let chunks =
        Parallel.map_p
          (fun chunk ->
            let chunk = List.map BLAKE2b.hash chunk in
            BLAKE2b.all chunk)
          chunks
      in
      let payload = BLAKE2b.all chunks in
      let json =
        yojson_of_payload_hash
          {
            author;
            level;
            previous;
            withdrawal_handles_hash;
            payload;
            tezos_operations;
          }
      in
      let json = Yojson.Safe.to_string json in
      BLAKE2b.hash json
  end

  (* TODO: we could avoid Yojson.Safe.to_string if we had locations *)
  let hash block =
    let block_payload_hash =
      let {
        author;
        level;
        previous;
        withdrawal_handles_hash;
        payload;
        tezos_operations;
      } =
        block
      in
      Payload_hash.hash ~author ~level ~previous ~withdrawal_handles_hash
        ~payload ~tezos_operations
    in
    let state_root_hash = BLAKE2b.hash "FIXME: we need to add the state root" in
    let block_hash =
      Block_hash.hash ~block_level:block.level ~block_payload_hash
        ~state_root_hash ~withdrawal_handles_hash:block.withdrawal_handles_hash
    in
    (block_payload_hash, block_hash)

  let t_of_yojson json =
    let { key; signature; block } = block_and_signature_of_yojson json in
    let {
      author;
      level;
      previous;
      payload;
      tezos_operations;
      withdrawal_handles_hash;
    } =
      block
    in
    (* TODO: serializing after deserializing *)
    let payload_hash, block_hash = hash block in

    (match Key_hash.(equal author (of_key key)) with
    | true -> ()
    | false -> raise Invalid_signature);
    (match
       let hash = Block_hash.to_blake2b block_hash in
       Signature.verify key signature hash
     with
    | true -> ()
    | false -> raise Invalid_signature);
    Block
      {
        key;
        signature;
        hash = block_hash;
        author;
        level;
        previous;
        payload;
        payload_hash;
        tezos_operations;
        withdrawal_handles_hash;
      }

  let yojson_of_t block =
    let (Block
          {
            key;
            signature;
            hash = _;
            author;
            level;
            previous;
            payload;
            payload_hash = _;
            tezos_operations;
            withdrawal_handles_hash;
          }) =
      block
    in
    let block =
      {
        author;
        level;
        previous;
        payload;
        tezos_operations;
        withdrawal_handles_hash;
      }
    in
    yojson_of_block_and_signature { key; signature; block }
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t

let produce ~identity ~level ~previous ~payload ~tezos_operations
    ~withdrawal_handles_hash =
  let author = Identity.key_hash identity in
  let payload_hash, block_hash =
    Repr.hash
      {
        author;
        level;
        previous;
        payload;
        tezos_operations;
        withdrawal_handles_hash;
      }
  in
  let key = Identity.key identity in
  let signature =
    let hash = Block_hash.to_blake2b block_hash in
    Identity.sign ~hash identity
  in
  Block
    {
      key;
      signature;
      hash = block_hash;
      author;
      level;
      previous;
      payload;
      payload_hash;
      tezos_operations;
      withdrawal_handles_hash;
    }

let sign ~identity block =
  let (Block { hash; _ }) = block in
  let hash = Block_hash.to_blake2b hash in
  Verified_signature.sign hash identity

let pp fmt (Block { hash; level; _ }) =
  let hash = Block_hash.to_b58 hash in
  let open Deku_stdlib in
  Format.fprintf fmt "Block [hash: %s, level: %a]" hash N.pp (Level.to_n level)

module Set = Set.Make (struct
  type t = block

  let compare = compare
  let t_of_yojson = t_of_yojson
  let yojson_of_t = yojson_of_t
end)
