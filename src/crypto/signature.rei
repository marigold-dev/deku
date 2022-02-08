type t =
  | Ed25519 of Ed25519.Signature.t 
  | Secp256k1 of Secp256k1.Signature.t 
  | P256 of P256.Signature.t [@@deriving (ord, eq)]
val sign : Secret.t -> BLAKE2B.t -> t
val verify : Key.t -> t -> BLAKE2B.t -> bool
val to_string : t -> string
val of_string : string -> t option
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result