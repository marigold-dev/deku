type signature =
  | Ed25519 of Ed25519.Signature.t
  | Secp256k1 of Secp256k1.Signature.t
  | P256 of P256.Signature.t

and t = signature [@@deriving eq, ord]

let of_b58 =
  let ed25519 string =
    match Ed25519.Signature.of_b58 string with
    | Some signature -> Some (Ed25519 signature)
    | None -> None
  in
  let secp256k1 string =
    match Secp256k1.Signature.of_b58 string with
    | Some signature -> Some (Secp256k1 signature)
    | None -> None
  in
  let p256 string =
    match P256.Signature.of_b58 string with
    | Some signature -> Some (P256 signature)
    | None -> None
  in
  fun string -> Base58.decode_variant [ ed25519; secp256k1; p256 ] string

let to_b58 = function
  | Ed25519 signature -> Ed25519.Signature.to_b58 signature
  | Secp256k1 signature -> Secp256k1.Signature.to_b58 signature
  | P256 signature -> P256.Signature.to_b58 signature

let zero = Ed25519 Ed25519.Signature.zero

let size =
  assert (
    Ed25519.Signature.size = Secp256k1.Signature.size
    && Secp256k1.Signature.size = P256.Signature.size);
  Ed25519.Signature.size

let sign secret hash =
  match secret with
  | Secret.Ed25519 secret -> Ed25519 (Ed25519.Signature.sign secret hash)
  | Secret.Secp256k1 secret -> Secp256k1 (Secp256k1.Signature.sign secret hash)
  | Secret.P256 secret -> P256 (P256.Signature.sign secret hash)

let verify key signature hash =
  match (key, signature) with
  | Key.Ed25519 key, Ed25519 signature ->
      Ed25519.Signature.verify key signature hash
  | Key.Secp256k1 key, Secp256k1 signature ->
      Secp256k1.Signature.verify key signature hash
  | Key.P256 key, P256 signature -> P256.Signature.verify key signature hash
  | ( (Key.Ed25519 _ | Key.Secp256k1 _ | Key.P256 _),
      (Ed25519 _ | Secp256k1 _ | P256 _) ) ->
      false
