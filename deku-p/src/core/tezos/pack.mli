open Deku_crypto

type t

val int : Z.t -> t
val string_ : string -> t
val nat : Z.t -> t
val bytes : bytes -> t
val pair : t -> t -> t
val list : t list -> t
val key : Key.t -> t
val key_hash : Key_hash.t -> t
val address : Address.t -> t
val to_bytes : t -> bytes
