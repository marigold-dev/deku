open Crypto
module Michelson_v1_primitives = Michelson_v1_primitives
type t
val int : Z.t -> t
val nat : Z.t -> t
val bytes : bytes -> t
val pair : t -> t -> t
val list : t list -> t
val key : Key.t -> t
val key_hash : Key_hash.t -> t
val address : Address.t -> t
val expr_encoding :
  Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical
  Data_encoding.t
val to_bytes : t -> bytes
