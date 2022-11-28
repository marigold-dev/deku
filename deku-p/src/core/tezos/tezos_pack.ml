open Deku_stdlib
open Deku_crypto
open Tezos_micheline
open Micheline
open Michelson_v1_primitives

type t = (canonical_location, prim) node

let int n = Int (-1, n)
let nat n = Int (-1, N.to_z n)
let bytes b = Bytes (-1, b)
let pair l r = Prim (-1, D_Pair, [ l; r ], [])
let list l = Seq (-1, l)
let key k = Bytes (-1, Data_encoding.Binary.to_bytes_exn Key.encoding k)

let key_hash h =
  Bytes (-1, Data_encoding.Binary.to_bytes_exn Key_hash.encoding h)

let address addr =
  Bytes (-1, Data_encoding.Binary.to_bytes_exn Tezos_address.encoding addr)

let to_bytes data =
  Data_encoding.Binary.to_bytes_exn Tezos_michelson.expr_encoding
    (strip_locations data)
  |> Bytes.cat (Bytes.of_string "\005")

let unpair_of_bytes bytes =
  let data = Bytes.sub_string bytes 1 (Bytes.length bytes - 1) in
  let%some michelson =
    Data_encoding.Binary.of_string_opt Tezos_michelson.expr_encoding data
  in
  let michelson = Micheline.root michelson in
  match michelson with
  | Prim (_, D_Pair, [ Bytes (_, l); Bytes (_, r) ], []) -> Some (l, r)
  | _ -> None
