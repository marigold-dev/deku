type t =
  | Ed25519   of Ed25519.Signature.t
  | Secp256k1 of Secp256k1.Signature.t
  | P256      of P256.Signature.t
[@@deriving ord, eq, yojson]

val size : int

val zero : t

val sign : Secret.t -> BLAKE2B.t -> t
val verify : Key.t -> t -> BLAKE2B.t -> bool

val to_raw : t -> string
val to_string : t -> string
val of_string : string -> t option
