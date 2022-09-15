open Deku_stdlib

type packed
type t = packed

val int : Z.t -> packed
val nat : N.t -> packed
val bytes : bytes -> packed
val pair : packed -> packed -> packed
val to_bytes : packed -> bytes
