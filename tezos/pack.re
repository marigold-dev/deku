open Crypto;
open Tezos_micheline;
open Micheline;
open Michelson_v1_primitives;

module Michelson_v1_primitives = Michelson_v1_primitives;

type t = node(canonical_location, prim);

let int = n => Int(-1, n);
let nat = n => Int(-1, n);
let bytes = b => Bytes(-1, b);
let pair = (l, r) => Prim(-1, D_Pair, [l, r], []);
let list = l => Seq(-1, l);
let key = k => Bytes(-1, Data_encoding.Binary.to_bytes_exn(Key.encoding, k));
let key_hash = h =>
  Bytes(-1, Data_encoding.Binary.to_bytes_exn(Key_hash.encoding, h));
let address = addr =>
  Bytes(-1, Data_encoding.Binary.to_bytes_exn(Address.encoding, addr));
let expr_encoding =
  Micheline.canonical_encoding_v1(
    ~variant="michelson_v1",
    Michelson_v1_primitives.prim_encoding,
  );
let to_bytes = data =>
  Data_encoding.Binary.to_bytes_exn(expr_encoding, strip_locations(data))
  |> Bytes.cat(Bytes.of_string("\005"));
