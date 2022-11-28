open Deku_stdlib
open Deku_crypto

type t

val int : Z.t -> t
val nat : N.t -> t
val bytes : bytes -> t
val pair : t -> t -> t
val list : t list -> t
val key : Key.t -> t
val key_hash : Key_hash.t -> t
val address : Tezos_address.t -> t
val to_bytes : t -> bytes
val unpair_of_bytes : bytes -> (bytes * bytes) option
