open Helpers
type t =
  | Ed25519   of Ed25519.Signature.t
  | Secp256k1 of Secp256k1.Signature.t
  | P256      of P256.Signature.t
[@@deriving ord, eq]

let zero = Ed25519 Ed25519.Signature.zero
let size =
  assert (
    Ed25519.Signature.size = Secp256k1.Signature.size
    && Secp256k1.Signature.size = P256.Signature.size);
  Ed25519.Signature.size

let sign secret hash =
  match secret with
  | Secret.Ed25519 secret -> Ed25519 (Ed25519.sign secret hash)
  | Secret.Secp256k1 secret -> Secp256k1 (Secp256k1.sign secret hash)
  | Secret.P256 secret -> P256 (P256.sign secret hash)
let verify key signature hash =
  match (key, signature) with
  | Key.Ed25519 key, Ed25519 signature -> Ed25519.verify key signature hash
  | Key.Secp256k1 key, Secp256k1 signature ->
    Secp256k1.verify key signature hash
  | Key.P256 key, P256 signature -> P256.verify key signature hash
  | ( (Key.Ed25519 _ | Key.Secp256k1 _ | Key.P256 _),
      (Ed25519 _ | Secp256k1 _ | P256 _) ) ->
    false

let to_raw = function
  | Ed25519 signature -> Ed25519.Signature.to_raw signature
  | Secp256k1 signature -> Secp256k1.Signature.to_raw signature
  | P256 signature -> P256.Signature.to_raw signature

let to_string = function
  | Ed25519 signature -> Ed25519.Signature.to_string signature
  | Secp256k1 signature -> Secp256k1.Signature.to_string signature
  | P256 signature -> P256.Signature.to_string signature
let of_string =
  let ed25519 string =
    let%some signature = Ed25519.Signature.of_string string in
    Some (Ed25519 signature) in
  let secp256k1 string =
    let%some signature = Secp256k1.Signature.of_string string in
    Some (Secp256k1 signature) in
  let p256 string =
    let%some signature = P256.Signature.of_string string in
    Some (P256 signature) in
  Encoding_helpers.parse_string_variant [ed25519; secp256k1; p256]
let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "signature" to_string of_string
