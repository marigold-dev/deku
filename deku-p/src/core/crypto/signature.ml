open Deku_repr

type signature =
  | Ed25519 of Ed25519.Signature.t
  | Secp256k1 of Secp256k1.Signature.t
  | P256 of P256.Signature.t

and t = signature [@@deriving eq, ord]

let encoding =
  let open Data_encoding in
  union
    [
      case ~title:"Ed25519" (Tag 0) Ed25519.Signature.encoding
        (function Ed25519 signature -> Some signature | _ -> None)
        (fun signature -> Ed25519 signature);
      case ~title:"Secp256k1" (Tag 1) Secp256k1.Signature.encoding
        (function Secp256k1 signature -> Some signature | _ -> None)
        (fun signature -> Secp256k1 signature);
      case ~title:"P256" (Tag 2) P256.Signature.encoding
        (function P256 signature -> Some signature | _ -> None)
        (fun signature -> P256 signature);
    ]

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
  fun string -> decode_variant [ ed25519; secp256k1; p256 ] string

let to_b58 = function
  | Ed25519 signature -> Ed25519.Signature.to_b58 signature
  | Secp256k1 signature -> Secp256k1.Signature.to_b58 signature
  | P256 signature -> P256.Signature.to_b58 signature

include With_yojson_of_b58 (struct
  type t = signature

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

let size =
  assert (
    Ed25519.Signature.size = Secp256k1.Signature.size
    && Secp256k1.Signature.size = P256.Signature.size);
  Ed25519.Signature.size

let key_encoding =
  let open Data_encoding in
  let to_pair (key, signature) =
    let signature =
      match signature with
      | Ed25519 signature ->
          Binary.to_bytes_exn Ed25519.Signature.encoding signature
      | Secp256k1 signature ->
          Binary.to_bytes_exn Secp256k1.Signature.encoding signature
      | P256 signature -> Binary.to_bytes_exn P256.Signature.encoding signature
    in
    (key, signature)
  in
  let of_pair (key, signature) =
    match key with
    | Key.Ed25519 _ ->
        let signature =
          Binary.of_bytes_exn Ed25519.Signature.encoding signature
        in
        (key, Ed25519 signature)
    | Key.Secp256k1 _ ->
        let signature =
          Binary.of_bytes_exn Secp256k1.Signature.encoding signature
        in
        (key, Secp256k1 signature)
    | Key.P256 _ ->
        let signature = Binary.of_bytes_exn P256.Signature.encoding signature in
        (key, P256 signature)
  in
  conv to_pair of_pair (tup2 Key.encoding (Fixed.bytes size))

let zero = Ed25519 Ed25519.Signature.zero

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

let show = to_b58
let pp fmt t = Format.pp_print_string fmt (to_b58 t)
