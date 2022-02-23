module Michelson_v1_primitives = Michelson_v1_primitives

type t = Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical

val expr_encoding : t Data_encoding.t

val unit : t
val is_unit : t -> bool
