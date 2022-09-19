open Tezos_micheline
module Michelson_v1_primitives = Michelson_v1_primitives

type t = Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical

let expr_encoding =
  Micheline_encoding.canonical_encoding ~variant:"michelson_v1"
    Michelson_v1_primitives.prim_encoding

let unit =
  Micheline.strip_locations (Prim (-1, Michelson_v1_primitives.D_Unit, [], []))

let is_unit value =
  match Micheline.root value with
  | Prim (_, Michelson_v1_primitives.D_Unit, [], []) -> true
  | _ -> false

type big_map_key = Key_hash of Deku_crypto.Key_hash.t
