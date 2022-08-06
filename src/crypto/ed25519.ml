open Deku_repr
open Mirage_crypto_ec
open Ed25519

module Secret = struct
  type secret = priv
  type t = secret

  let equal a b =
    let a, b = (priv_to_cstruct a, priv_to_cstruct b) in
    Cstruct.equal a b

  let compare a b = Cstruct.compare (priv_to_cstruct a) (priv_to_cstruct b)

  include With_b58_and_yojson (struct
    type t = secret

    let prefix = Prefix.ed25519_seed
    let to_raw secret = Cstruct.to_string (priv_to_cstruct secret)

    let of_raw string =
      priv_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  let generate () =
    let secret, _key = Ed25519.generate () in
    secret
end

module Key = struct
  type key = pub
  type t = key

  let equal a b =
    let a, b = (pub_to_cstruct a, pub_to_cstruct b) in
    Cstruct.equal a b

  let compare a b = Cstruct.compare (pub_to_cstruct a) (pub_to_cstruct b)
  let of_secret secret = pub_of_priv secret
  let to_raw key = Cstruct.to_string (pub_to_cstruct key)

  include With_b58_and_yojson (struct
    type t = key

    let prefix = Prefix.ed25519_public_key
    let to_raw = to_raw

    let of_raw string =
      pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  module Encoding = Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "Ed25519.Public_key"
    let title = "Ed25519 public key"
    let size = 32
    let prefix = Base58.Prefix.ed25519_public_key
    let to_raw t = Cstruct.to_string (Ed25519.pub_to_cstruct t)

    let of_raw string =
      Ed25519.pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)
end

module Key_hash = struct
  open BLAKE2b.BLAKE2b_160

  type key_hash = BLAKE2b.BLAKE2b_160.t
  type t = key_hash

  let equal = equal
  let compare = compare
  let of_key key = hash (Key.to_raw key)

  include With_b58 (struct
    let prefix = Prefix.ed25519_public_key_hash
  end)

  include With_yojson_of_b58 (struct
    type t = key_hash

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)

  module Encoding = Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "Ed25519.Public_key_hash"
    let title = "An Ed25519 public key hash"
    let size = BLAKE2b.BLAKE2b_160.digest_size
    let prefix = Base58.Prefix.ed25519_public_key_hash
    let to_raw = BLAKE2b.BLAKE2b_160.to_raw
    let of_raw = BLAKE2b.BLAKE2b_160.of_raw
  end)

  let encoding = Encoding.encoding
end

module Signature = struct
  type signature = string
  type t = signature

  let equal = String.equal
  let compare = String.compare
  let size = 64
  let zero = String.make size '\x00'

  include With_b58_and_yojson (struct
    type t = signature

    let prefix = Prefix.ed25519_signature
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
      let signature = sign ~key:secret hash in
      Cstruct.to_string signature

    let verify key signature hash =
      let hash = Cstruct.of_string hash in
      let signature = Cstruct.of_string signature in
      verify ~key ~msg:hash signature
  end)
end
