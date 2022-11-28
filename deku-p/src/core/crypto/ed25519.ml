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

  include With_b58_and_encoding_and_yojson (struct
    type t = secret

    let name = "Ed25519.Secret_key"
    let prefix = Prefix.ed25519_seed
    let size = 32
    let to_raw secret = Cstruct.to_string (priv_to_cstruct secret)

    let of_raw string =
      priv_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  let generate () =
    let secret, _key = Ed25519.generate () in
    secret

  let cmdliner_converter =
    let of_string s =
      match of_b58 s with
      | Some x -> `Ok x
      | None -> `Error "Could not parse ED25519 secret"
    in
    let to_string fmt t = Format.fprintf fmt "%s" (to_b58 t) in
    (of_string, to_string)
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

  include With_b58_and_encoding_and_yojson (struct
    type t = key

    let name = "Ed25519.Public_key"
    let prefix = Prefix.ed25519_public_key
    let size = 32
    let to_raw = to_raw

    let of_raw string =
      pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  let cmdliner_converter =
    let of_string s =
      match of_b58 s with
      | Some x -> `Ok x
      | None ->
          `Error (Format.sprintf "Could not parse '%s' as a ED25519 key" s)
    in
    let to_string fmt t = Format.fprintf fmt "%s" (to_b58 t) in
    (of_string, to_string)
end

module Key_hash = struct
  open BLAKE2b.BLAKE2b_160

  type key_hash = BLAKE2b.BLAKE2b_160.t
  type t = key_hash

  let zero = zero
  let equal = equal
  let compare = compare
  let of_key key = hash (Key.to_raw key)

  include With_b58_and_encoding_and_yojson (struct
    let name = "Ed25519.Public_key_hash"
    let prefix = Prefix.ed25519_public_key_hash
  end)
end

module Signature = struct
  type signature = string
  type t = signature

  let equal = String.equal
  let compare = String.compare
  let size = 64
  let zero = String.make size '\x00'

  include With_b58_and_encoding_and_yojson (struct
    type t = signature

    let name = "Ed25519.Signature"
    let prefix = Prefix.ed25519_signature
    let size = size
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
