(* TODO: make those GADTs *)
type secret =
  | Ed25519 of Ed25519.Secret.t
  | Secp256k1 of Secp256k1.Secret.t
  | P256 of P256.Secret.t

type t = secret [@@deriving eq, ord]

val secret_encoding : t Data_encoding.t

(* repr *)
val of_b58 : string -> secret option
val to_b58 : secret -> string
