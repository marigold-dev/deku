open Helpers;
open Tezos_micheline;
open Micheline;
open Michelson_v1_primitives;

let expr_encoding =
  Micheline.canonical_encoding_v1(
    ~variant="michelson_v1",
    Michelson_v1_primitives.prim_encoding,
  );

let int = n => Int(-1, Z.of_int64(n));
let hash = b => Bytes(-1, BLAKE2B.to_raw_string(b) |> Bytes.of_string);
let list = l => Seq(-1, l);
let pair = (l, r) => Prim(-1, D_Pair, [l, r], []);
let key = k => Bytes(-1, Ed25519.Public_key.to_tezos_bytes(k));
let block_hash_structure =
    (~block_height, ~block_payload_hash, ~state_hash, ~validators_hash) =>
  pair(
    pair(int(block_height), hash(block_payload_hash)),
    pair(hash(state_hash), hash(validators_hash)),
  );

let pack = data =>
  Data_encoding.Binary.to_bytes_exn(expr_encoding, strip_locations(data))
  |> Bytes.cat(Bytes.of_string("\005"))
  |> Bytes.to_string;

let hash_validators = validators =>
  BLAKE2B.hash(pack(list(List.map(key, validators))));

let hash_block =
    (~block_height, ~block_payload_hash, ~state_hash, ~validators_hash) =>
  pack(
    block_hash_structure(
      ~block_height,
      ~block_payload_hash,
      ~state_hash,
      ~validators_hash,
    ),
  )
  |> BLAKE2B.hash;
