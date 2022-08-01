open Deku_crypto

exception Invalid_hash
exception Invalid_signature

type verified_signature
type t = verified_signature [@@deriving eq, ord, yojson]

val sign : BLAKE2b.t -> Identity.t -> verified_signature
val verify : BLAKE2b.t -> Key.t -> Signature.t -> verified_signature option
val key : verified_signature -> Key.t
val key_hash : verified_signature -> Key_hash.t
val signed_hash : verified_signature -> BLAKE2b.t
val signature : verified_signature -> Signature.t

module Set : Set.S with type elt = verified_signature
