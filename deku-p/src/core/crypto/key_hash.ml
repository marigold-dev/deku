open Deku_stdlib
open Deku_repr

type key_hash =
  | Ed25519 of Ed25519.Key_hash.t
  | Secp256k1 of Secp256k1.Key_hash.t
  | P256 of P256.Key_hash.t

and t = key_hash [@@deriving eq, ord, yojson]

let of_b58 =
  let ed25519 string =
    match Ed25519.Key_hash.of_b58 string with
    | Some key_hash -> Some (Ed25519 key_hash)
    | None -> None
  in
  let secp256k1 string =
    match Secp256k1.Key_hash.of_b58 string with
    | Some key_hash -> Some (Secp256k1 key_hash)
    | None -> None
  in
  let p256 string =
    match P256.Key_hash.of_b58 string with
    | Some key_hash -> Some (P256 key_hash)
    | None -> None
  in
  fun string -> decode_variant [ ed25519; secp256k1; p256 ] string

let to_b58 key_hash =
  match key_hash with
  | Ed25519 key_hash -> Ed25519.Key_hash.to_b58 key_hash
  | Secp256k1 key_hash -> Secp256k1.Key_hash.to_b58 key_hash
  | P256 key_hash -> P256.Key_hash.to_b58 key_hash

let of_key = function
  | Key.Ed25519 key -> Ed25519 (Ed25519.Key_hash.of_key key)
  | Key.Secp256k1 key -> Secp256k1 (Secp256k1.Key_hash.of_key key)
  | Key.P256 key -> P256 (P256.Key_hash.of_key key)

include With_yojson_of_b58 (struct
  type t = key_hash

  let of_b58 = of_b58
  let to_b58 = to_b58
end)

let encoding =
  let open Data_encoding in
  let name = "Signature.Public_key_hash" in
  let raw_encoding =
    union
      [
        case (Tag 0) Ed25519.Key_hash.encoding ~title:"Ed25519"
          (function Ed25519 key_hash -> Some key_hash | _ -> None)
          (fun key_hash -> Ed25519 key_hash);
        case (Tag 1) Secp256k1.Key_hash.encoding ~title:"Secp256k1"
          (function Secp256k1 x -> Some x | _ -> None)
          (fun x -> Secp256k1 x);
        case (Tag 2) P256.Key_hash.encoding ~title:"P256"
          (function P256 x -> Some x | _ -> None)
          (fun x -> P256 x);
      ]
  in
  make_encoding ~name ~to_string:to_b58 ~of_string:of_b58 ~raw_encoding

module Map = Map.Make (struct
  type t = key_hash [@@deriving ord, yojson]

  let encoding = encoding
end)

module Set = Set.Make (struct
  type t = key_hash [@@deriving ord, yojson]

  let encoding = encoding
end)

let cmdliner_converter =
  let of_string s =
    match of_b58 s with
    | Some s -> `Ok s
    | None -> `Error (Format.sprintf "Could not parse '%s' as a key hash" s)
  in
  let to_string fmt t = Format.fprintf fmt "%s" (to_b58 t) in
  (of_string, to_string)

let show = to_b58
let pp fmt t = Format.pp_print_string fmt (to_b58 t)
