module Michelson_v1_primitives = Michelson_v1_primitives

type michelson =
  Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical

type t = michelson

val expr_encoding : michelson Data_encoding.t
