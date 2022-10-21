open Deku_repr

type key =
  | Ed25519 of Ed25519.Key.t
  | Secp256k1 of Secp256k1.Key.t
  | P256 of P256.Key.t

and t = key [@@deriving eq, ord]

let of_b58 =
  let ed25519 string =
    match Ed25519.Key.of_b58 string with
    | Some key -> Some (Ed25519 key)
    | None -> None
  in
  let secp256k1 string =
    match Secp256k1.Key.of_b58 string with
    | Some key -> Some (Secp256k1 key)
    | None -> None
  in
  let p256 string =
    match P256.Key.of_b58 string with
    | Some key -> Some (P256 key)
    | None -> None
  in
  fun string -> decode_variant [ ed25519; secp256k1; p256 ] string

let to_b58 key =
  match key with
  | Ed25519 key -> Ed25519.Key.to_b58 key
  | Secp256k1 key -> Secp256k1.Key.to_b58 key
  | P256 key -> P256.Key.to_b58 key

let of_secret secret =
  match secret with
  | Secret.Ed25519 secret -> Ed25519 (Ed25519.Key.of_secret secret)
  | Secret.Secp256k1 secret -> Secp256k1 (Secp256k1.Key.of_secret secret)
  | Secret.P256 secret -> P256 (P256.Key.of_secret secret)

include With_yojson_of_b58 (struct
  type t = key

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

let encoding =
  let open Data_encoding in
  let name = "Signature.Public_key" in
  let raw_encoding =
    union
      [
        case (Tag 0) Ed25519.Key.encoding ~title:"Ed25519"
          (function Ed25519 key -> Some key | _ -> None)
          (fun key -> Ed25519 key);
        case (Tag 1) Secp256k1.Key.encoding ~title:"Secp256k1"
          (function Secp256k1 x -> Some x | _ -> None)
          (fun x -> Secp256k1 x);
        case (Tag 2) P256.Key.encoding ~title:"P256"
          (function P256 x -> Some x | _ -> None)
          (fun x -> P256 x);
      ]
  in
  make_encoding ~name ~to_string:to_b58 ~of_string:of_b58 ~raw_encoding

let show = to_b58
let pp fmt t = Format.pp_print_string fmt (to_b58 t)
