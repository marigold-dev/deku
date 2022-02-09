open Helpers
type t =
  | Ed25519   of Ed25519.Key.t
  | Secp256k1 of Secp256k1.Key.t
  | P256      of P256.Key.t
[@@deriving ord, eq]
let of_secret = function
  | Secret.Ed25519 secret -> Ed25519 (Ed25519.Key.of_secret secret)
  | Secret.Secp256k1 secret -> Secp256k1 (Secp256k1.Key.of_secret secret)
  | Secret.P256 secret -> P256 (P256.Key.of_secret secret)
let to_string = function
  | Ed25519 key -> Ed25519.Key.to_string key
  | Secp256k1 key -> Secp256k1.Key.to_string key
  | P256 key -> P256.Key.to_string key
let of_string =
  let ed25519 string =
    let%some key = Ed25519.Key.of_string string in
    Some (Ed25519 key) in
  let secp256k1 string =
    let%some key = Secp256k1.Key.of_string string in
    Some (Secp256k1 key) in
  let p256 string =
    let%some key = P256.Key.of_string string in
    Some (P256 key) in
  Encoding_helpers.parse_string_variant [ed25519; secp256k1; p256]
let encoding =
  let open Data_encoding in
  let name = "Signature.Public_key" in
  let title = "A Ed25519, Secp256k1, or P256 public key" in
  let raw_encoding =
    def "public_key" ~description:title
    @@ union
         [
           case (Tag 0) Ed25519.Key.encoding ~title:"Ed25519"
             (function
               | Ed25519 key -> Some key
               | _ -> None)
             (fun key -> Ed25519 key);
           case (Tag 1) Secp256k1.Key.encoding ~title:"Secp256k1"
             (function
               | Secp256k1 x -> Some x
               | _ -> None)
             (fun x -> Secp256k1 x);
           case (Tag 2) P256.Key.encoding ~title:"P256"
             (function
               | P256 x -> Some x
               | _ -> None)
             (fun x -> P256 x);
         ] in
  Encoding_helpers.make_encoding ~name ~title ~to_string ~of_string
    ~raw_encoding
let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "key" to_string of_string
