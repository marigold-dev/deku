type t =
  | Ed25519   of Ed25519.Key.t
  | Secp256k1 of Secp256k1.Key.t
  | P256      of P256.Key.t
[@@deriving ord, eq]
val of_secret : Secret.t -> t
val encoding : t Data_encoding.t
val to_string : t -> string
val of_string : string -> t option
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
