open Deku_crypto
open Deku_concepts
open Deku_protocol

type block =
  | Block of {
      key : Key.t;
      signature : Signature.t;
      hash : Block_hash.t;
      withdrawal_handles_hash : BLAKE2b.t;
      author : Key_hash.t;
      level : Level.t;
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

module Set = Set.Make (struct
  type t = block

  let compare = compare
end)

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

  (* TODO: we could avoid Yojson.Safe.to_string if we had locations *)
  let hash block =
    let block_payload_hash =
      yojson_of_block block |> Yojson.Safe.to_string |> BLAKE2b.hash
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

let payload_of_operations ~parallel_map operations =
  (* TODO: this likely should not be here *)
  parallel_map
    (fun operation ->
      let json = Operation.yojson_of_t operation in
      Yojson.Safe.to_string json)
    operations

let produce ~parallel_map ~identity ~level ~previous ~operations
    ~tezos_operations ~withdrawal_handles_hash =
  let author = Identity.key_hash identity in
  let payload = payload_of_operations ~parallel_map operations in
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

let pp fmt (Block { hash; level; _ }) =
  let hash = Block_hash.to_blake2b hash in
  let open Deku_stdlib in
  Format.fprintf fmt "Block [hash: %a, level: %a]" BLAKE2b.pp hash N.pp
    (Level.to_n level)

let sign ~identity block =
  let (Block { hash; _ }) = block in
  let hash = Block_hash.to_blake2b hash in
  Verified_signature.sign hash identity
