open Deku_stdlib
open Deku_crypto
open Deku_concepts
open Deku_protocol
open Deku_ledger

type block =
  | Block of {
      key : Key.t;
      signature : Signature.t;
      hash : Block_hash.t;
      author : Key_hash.t;
      level : Level.t;
      (* TODO: nonce *)
      previous : Block_hash.t;
      tezos_operations : Tezos_operation.t list;
      withdrawal_handles_hash : Ledger.Withdrawal_handle.hash;
      payload_hash : BLAKE2b.t;
      payload : string;
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

let header_encoding =
  let open Data_encoding in
  tup5 Key_hash.encoding Level.encoding Block_hash.encoding
    (list Tezos_operation.encoding)
    Ledger.Withdrawal_handle.hash_encoding

let hash ~author ~level ~previous ~tezos_operations ~withdrawal_handles_hash
    ~payload =
  let payload =
    let payload = BLAKE2b.hash payload in
    let header =
      Data_encoding.Binary.to_string_exn header_encoding
        (author, level, previous, tezos_operations, withdrawal_handles_hash)
    in
    let header = BLAKE2b.hash header in
    BLAKE2b.both header payload
  in
  let hash =
    let state_root_hash = BLAKE2b.hash "FIXME: we need to add the state root" in
    Block_hash.hash ~block_level:level ~block_payload_hash:payload
      ~state_root_hash ~withdrawal_handles_hash
  in
  (payload, hash)

let encoding =
  let open Data_encoding in
  conv_with_guard
    (fun block ->
      let (Block
            {
              key;
              signature;
              hash = _;
              author;
              level;
              previous;
              tezos_operations;
              withdrawal_handles_hash;
              payload;
              payload_hash = _;
            }) =
        block
      in
      ( (key, signature),
        (author, level, previous, tezos_operations, withdrawal_handles_hash),
        payload ))
    (fun ( (key, signature),
           (author, level, previous, tezos_operations, withdrawal_handles_hash),
           payload ) ->
      let payload_hash, hash =
        hash ~author ~level ~previous ~tezos_operations ~withdrawal_handles_hash
          ~payload
      in
      match
        Key_hash.(equal author (of_key key))
        &&
        let hash = Block_hash.to_blake2b hash in
        Signature.verify key signature hash
      with
      | true ->
          let block =
            Block
              {
                key;
                signature;
                hash;
                author;
                level;
                previous;
                tezos_operations;
                withdrawal_handles_hash;
                payload;
                payload_hash;
              }
          in
          Ok block
      | false -> Error "Invalid_signature")
    (tup3 Signature.key_encoding header_encoding string)

let t_of_yojson json =
  let json = Yojson.Safe.to_string json in
  let json = Result.get_ok (Data_encoding.Json.from_string json) in
  Data_encoding.Json.destruct encoding json

let yojson_of_t signed =
  let json = Data_encoding.Json.construct encoding signed in
  let json = Data_encoding.Json.to_string json in
  Yojson.Safe.from_string json

let produce ~identity ~level ~previous ~payload ~tezos_operations
    ~withdrawal_handles_hash =
  let payload = Payload.encode ~payload in
  let author = Identity.key_hash identity in
  let payload_hash, block_hash =
    hash ~author ~level ~previous ~tezos_operations ~withdrawal_handles_hash
      ~payload
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
  let encoding = encoding
end)
