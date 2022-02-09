type t =
  | Ed25519   of Ed25519.Key_hash.t
  | Secp256k1 of Secp256k1.Key_hash.t
  | P256      of P256.Key_hash.t
[@@deriving ord, eq]
val of_key : Key.t -> t
val matches_key : Key.t -> t -> bool
val make_ed25519 : unit -> Secret.t * Key.t * t
val encoding : t Data_encoding.t
val to_string : t -> string
val of_string : string -> t option
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
