(* TODO: make those GADTs *)
type signature =
  | Ed25519 of Ed25519.Signature.t
  | Secp256k1 of Secp256k1.Signature.t
  | P256 of P256.Signature.t

type t = signature [@@deriving eq, ord, show, yojson]

val encoding : t Data_encoding.t

(* repr *)
val of_b58 : string -> signature option
val to_b58 : signature -> string
val key_encoding : (Key.t * signature) Data_encoding.t

(* utils *)
val zero : signature
val size : int

(* operations *)
val sign : Secret.t -> BLAKE2b.t -> signature
val verify : Key.t -> signature -> BLAKE2b.t -> bool
