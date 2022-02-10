open Tezos_micheline

module Michelson_v1_primitives = Michelson_v1_primitives

type t = Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical

let expr_encoding =
  Micheline_encoding.canonical_encoding ~variant:"michelson_v1"
    Michelson_v1_primitives.prim_encoding
let lazy_expr_encoding = Data_encoding.lazy_encoding expr_encoding
