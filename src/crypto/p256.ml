open Deku_repr
open Mirage_crypto_ec
open P256
open P256.Dsa

module Secret = struct
  type secret = Dsa.priv
  type t = secret

  let equal a b =
    let a, b = (priv_to_cstruct a, priv_to_cstruct b) in
    Cstruct.equal a b

  let compare a b = Cstruct.compare (priv_to_cstruct a) (priv_to_cstruct b)

  include With_all_encodings (struct
    type t = secret

    let name = "P256.Secret_key"
    let title = "A P256 secret key"
    let size = 32
    let prefix = Prefix.p256_secret_key
    let to_raw secret = Cstruct.to_string (priv_to_cstruct secret)

    let of_raw string =
      priv_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  let generate () =
    let secret, _key = Dsa.generate () in
    secret
end

module Key = struct
  type key = pub
  type t = key

  let of_secret = pub_of_priv

  let equal a b =
    let a, b = (pub_to_cstruct a, pub_to_cstruct b) in
    Cstruct.equal a b

  let compare a b = Cstruct.compare (pub_to_cstruct a) (pub_to_cstruct b)
  let to_raw key = Cstruct.to_string (pub_to_cstruct ~compress:true key)

  include With_all_encodings (struct
    type t = key

    let name = "P256.Public_key"
    let title = "P256 public key"
    let size = 33
    let prefix = Prefix.p256_public_key
    let to_raw = to_raw

    let of_raw string =
      pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)
end

module Key_hash = struct
  open BLAKE2b.BLAKE2b_160

  type key_hash = BLAKE2b.BLAKE2b_160.t
  type t = key_hash

  let equal = equal
  let compare = compare
  let of_key key = hash (Key.to_raw key)

  include With_all_encodings (struct
    let name = "P256.Public_key_hash"
    let title = "An P256 public key hash"
    let prefix = Prefix.p256_public_key_hash
  end)
end

module Signature = struct
  type signature = string
  type t = signature

  let equal = String.equal
  let compare = String.compare
  let size = 64
  let zero = String.make size '\x00'

  include With_all_encodings (struct
    type t = signature

    let name = "P256"
    let title = "An P256 signature"
    let size = 64
    let prefix = Prefix.p256_signature
    let to_raw signature = signature

    let of_raw string =
      match String.length string = size with
      | true -> Some string
      | false -> None
  end)

  include BLAKE2b.With_alg (struct
    type secret = Secret.t
    type key = Key.t
    type signature = t

    let sign secret hash =
      let hash = Cstruct.of_string hash in
      (* TODO: explain r and s*)
      let r, s = sign ~key:secret hash in
      let signature = Cstruct.append r s in
      Cstruct.to_string signature

    let verify key signature hash =
      let r, s = (String.sub signature 0 32, String.sub signature 32 32) in
      let r, s = (Cstruct.of_string r, Cstruct.of_string s) in
      let hash = Cstruct.of_string hash in
      verify ~key (r, s) hash
  end)
end
