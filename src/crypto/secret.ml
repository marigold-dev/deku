open Deku_repr

type secret =
  | Ed25519 of Ed25519.Secret.t
  | Secp256k1 of Secp256k1.Secret.t
  | P256 of P256.Secret.t

and t = secret [@@deriving eq, ord]

include With_all_encodings_of_many (struct
  type t = secret

  let key = "secret_key"
  let name = "Signature.Secret_key"
  let title = "A Ed25519, Secp256k1 or P256 secret key"

  let cases =
    let open Data_encoding in
    [
      case (Tag 0) Ed25519.Secret.encoding ~title:"Ed25519"
        (function Ed25519 secret -> Some secret | _ -> None)
        (fun secret -> Ed25519 secret);
      case (Tag 1) Secp256k1.Secret.encoding ~title:"Secp256k1"
        (function Secp256k1 secret -> Some secret | _ -> None)
        (fun secret -> Secp256k1 secret);
      case (Tag 2) P256.Secret.encoding ~title:"P256"
        (function P256 secret -> Some secret | _ -> None)
        (fun secret -> P256 secret);
    ]

  let of_b58 =
    let ed25519 string =
      match Ed25519.Secret.of_b58 string with
      | Some secret -> Some (Ed25519 secret)
      | None -> None
    in
    let secp256k1 string =
      match Secp256k1.Secret.of_b58 string with
      | Some secret -> Some (Secp256k1 secret)
      | None -> None
    in
    let p256 string =
      match P256.Secret.of_b58 string with
      | Some secret -> Some (P256 secret)
      | None -> None
    in
    [ ed25519; secp256k1; p256 ]

  let to_b58 secret =
    match secret with
    | Ed25519 secret -> Ed25519.Secret.to_b58 secret
    | Secp256k1 secret -> Secp256k1.Secret.to_b58 secret
    | P256 secret -> P256.Secret.to_b58 secret
end)
