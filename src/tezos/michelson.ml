open Tezos_micheline
module Michelson_v1_primitives = Michelson_v1_primitives

type michelson =
  Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical

type t = michelson

let expr_encoding =
  Micheline_encoding.canonical_encoding ~variant:"michelson_v1"
    Michelson_v1_primitives.prim_encoding
