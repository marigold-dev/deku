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
      payload : string;
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

exception Failed_to_decode

module Repr = struct
  type header = {
    key : Key.t;
    signature : Signature.t;
    author : Key_hash.t;
    level : Level.t;
    previous : Block_hash.t;
    withdrawal_handles_hash : Ledger.Withdrawal_handle.hash;
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
      let payload = BLAKE2b.hash payload in
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

  let hash ~author ~level ~previous ~withdrawal_handles_hash ~tezos_operations
      ~payload =
    let block_payload_hash =
      Payload_hash.hash ~author ~level ~previous ~withdrawal_handles_hash
        ~payload ~tezos_operations
    in
    let state_root_hash = BLAKE2b.hash "FIXME: we need to add the state root" in
    let block_hash =
      Block_hash.hash ~block_level:level ~block_payload_hash ~state_root_hash
        ~withdrawal_handles_hash
    in
    (block_payload_hash, block_hash)

  let encode block =
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
    let header =
      {
        key;
        signature;
        author;
        level;
        previous;
        withdrawal_handles_hash;
        tezos_operations;
      }
    in
    let json = yojson_of_header header in
    [ Yojson.Safe.to_string json; payload ]

  let decode fragments =
    let json, payload =
      match fragments with
      | [ json; payload ] -> (json, payload)
      | _ -> raise Failed_to_decode
    in
    let json = Yojson.Safe.from_string json in
    let header = header_of_yojson json in
    let {
      key;
      signature;
      author;
      level;
      previous;
      tezos_operations;
      withdrawal_handles_hash;
    } =
      header
    in
    let payload_hash, hash =
      hash ~author ~level ~previous ~withdrawal_handles_hash ~tezos_operations
        ~payload
    in
    (match Key_hash.(equal author (of_key key)) with
    | true -> ()
    | false -> raise Failed_to_decode);
    (match
       let hash = Block_hash.to_blake2b hash in
       Signature.verify key signature hash
     with
    | true -> ()
    | false -> raise Failed_to_decode);

    Block
      {
        key;
        signature;
        hash;
        author;
        level;
        previous;
        payload;
        payload_hash;
        tezos_operations;
        withdrawal_handles_hash;
      }

  let t_of_yojson json =
    let fragments = [%of_yojson: string list] json in
    decode fragments

  let yojson_of_t block =
    let fragments = encode block in
    [%yojson_of: string list] fragments
end

let t_of_yojson = Repr.t_of_yojson
let yojson_of_t = Repr.yojson_of_t
let encode = Repr.encode
let decode = Repr.decode

let produce ~identity ~level ~previous ~payload ~tezos_operations
    ~withdrawal_handles_hash =
  let payload = Payload.encode ~payload in
  let author = Identity.key_hash identity in
  let payload_hash, block_hash =
    Repr.hash ~author ~level ~previous ~payload ~tezos_operations
      ~withdrawal_handles_hash
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
