open Deku_crypto

(* TODO: probably should be under concepts *)
type identity
type t = identity

val make : Secret.t -> identity
val key : identity -> Key.t
val key_hash : identity -> Key_hash.t
val sign : hash:BLAKE2b.t -> identity -> Signature.t
