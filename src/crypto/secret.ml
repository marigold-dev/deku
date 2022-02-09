open Helpers
type t =
  | Ed25519   of Ed25519.Secret.t
  | Secp256k1 of Secp256k1.Secret.t
  | P256      of P256.Secret.t
[@@deriving ord, eq]
let to_string = function
  | Ed25519 secret -> Ed25519.Secret.to_string secret
  | Secp256k1 secret -> Secp256k1.Secret.to_string secret
  | P256 secret -> P256.Secret.to_string secret
let of_string =
  let ed25519 string =
    let%some secret = Ed25519.Secret.of_string string in
    Some (Ed25519 secret) in
  let secp256k1 string =
    let%some secret = Secp256k1.Secret.of_string string in
    Some (Secp256k1 secret) in
  let p256 string =
    let%some secret = P256.Secret.of_string string in
    Some (P256 secret) in
  Encoding_helpers.parse_string_variant [ed25519; secp256k1; p256]
let encoding =
  let open Data_encoding in
  let name = "Signature.Secret_key" in
  let title = "A Ed25519, Secp256k1 or P256 secret key" in
  let raw_encoding =
    def "secret_key" ~description:title
    @@ union
         [
           case (Tag 0) Ed25519.Secret.encoding ~title:"Ed25519"
             (function
               | Ed25519 secret -> Some secret
               | _ -> None)
             (fun secret -> Ed25519 secret);
           case (Tag 1) Secp256k1.Secret.encoding ~title:"Secp256k1"
             (function
               | Secp256k1 secret -> Some secret
               | _ -> None)
             (fun secret -> Secp256k1 secret);
           case (Tag 2) P256.Secret.encoding ~title:"P256"
             (function
               | P256 secret -> Some secret
               | _ -> None)
             (fun secret -> P256 secret);
         ] in
  Encoding_helpers.make_encoding ~name ~title ~to_string ~of_string
    ~raw_encoding
let to_yojson, of_yojson =
  Yojson_ext.with_yojson_string "secret" to_string of_string
