open Mirage_crypto_ec
open P256
open Helpers
open P256.Dsa
module Secret = struct
  type t = Dsa.priv
  let equal a b =
    let a, b = (priv_to_cstruct a, priv_to_cstruct b) in
    Cstruct.equal a b
  let compare a b = Cstruct.compare (priv_to_cstruct a) (priv_to_cstruct b)
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "P256.Secret_key"
    let title = "A P256 secret key"
    let size = 32
    let prefix = Base58.Prefix.p256_secret_key
    let to_raw t = Cstruct.to_string (priv_to_cstruct t)
    let of_raw string =
      priv_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)
end
module Key = struct
  type t = pub
  let of_secret = pub_of_priv
  let equal a b =
    let a, b = (pub_to_cstruct a, pub_to_cstruct b) in
    Cstruct.equal a b
  let compare a b = Cstruct.compare (pub_to_cstruct a) (pub_to_cstruct b)
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "P256.Public_key"
    let title = "P256 public key"
    let size = 33
    let prefix = Base58.Prefix.p256_public_key
    let to_raw t = Cstruct.to_string (pub_to_cstruct ~compress:true t)
    let of_raw string =
      pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)
end
module Key_hash = struct
  type t = BLAKE2B_20.t [@@deriving ord, eq]
  let of_key t =
    BLAKE2B_20.hash (pub_to_cstruct ~compress:true t |> Cstruct.to_string)
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "P256.Public_key_hash"
    let title = "An P256 public key hash"
    let size = BLAKE2B_20.size
    let prefix = Base58.Prefix.p256_public_key_hash
    let to_raw = BLAKE2B_20.to_raw_string
    let of_raw = BLAKE2B_20.of_raw_string
  end)
end
module Signature = struct
  type t = string [@@deriving ord, eq]
  include Encoding_helpers.Make_b58 (struct
    type nonrec t = t
    let name = "P256"
    let title = "An P256 signature"
    let size = 64
    let prefix = Base58.Prefix.p256_signature
    let to_raw = Fun.id
    let of_raw string =
      match String.length string = size with
      | true -> Some string
      | false -> None
  end)
  let zero = String.make size '\x00'
end
let sign secret hash =
  let r, s =
    Cstruct.of_string (BLAKE2B.to_raw_string hash) |> sign ~key:secret in
  [r; s] |> Cstruct.concat |> Cstruct.to_string
let verify public signature hash =
  let r, s = (String.sub signature 0 32, String.sub signature 32 32) in
  verify ~key:public
    (Cstruct.of_string r, Cstruct.of_string s)
    (Cstruct.of_string (BLAKE2B.to_raw_string hash))
let generate () = Dsa.generate ()
