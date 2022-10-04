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

  include With_b58_and_yojson (struct
    type t = secret

    let prefix = Prefix.p256_secret_key
    let to_raw secret = Cstruct.to_string (priv_to_cstruct secret)

    let of_raw string =
      priv_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  let generate () =
    let secret, _key = Dsa.generate () in
    secret

  let cmdliner_converter =
    let of_string s =
      match of_b58 s with
      | Some x -> `Ok x
      | None -> `Error "Could not parse P256 secret"
    in
    let to_string fmt t = Format.fprintf fmt "%s" (to_b58 t) in
    (of_string, to_string)
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

  include With_b58_and_yojson (struct
    type t = key

    let prefix = Prefix.p256_public_key
    let to_raw = to_raw

    let of_raw string =
      pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  module Encoding = Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "P256.Public_key"
    let title = "P256 public key"
    let size = 33
    let prefix = Base58.Prefix.p256_public_key
    let to_raw t = Cstruct.to_string (pub_to_cstruct ~compress:true t)

    let of_raw string =
      pub_of_cstruct (Cstruct.of_string string) |> Result.to_option
  end)

  let encoding = Encoding.encoding

  let cmdliner_converter =
    let of_string s =
      match of_b58 s with
      | Some x -> `Ok x
      | None -> `Error (Format.sprintf "Could not parse '%s' as a P256 key" s)
    in
    let to_string fmt t = Format.fprintf fmt "%s" (to_b58 t) in
    (of_string, to_string)
end

module Key_hash = struct
  open BLAKE2b.BLAKE2b_160

  type key_hash = BLAKE2b.BLAKE2b_160.t
  type t = key_hash

  let equal = equal
  let compare = compare
  let of_key key = hash (Key.to_raw key)

  include With_b58 (struct
    let prefix = Prefix.p256_public_key_hash
  end)

  include With_yojson_of_b58 (struct
    type t = key_hash

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)

  module Encoding = Encoding_helpers.Make_b58 (struct
    type nonrec t = t

    let name = "P256.Public_key_hash"
    let title = "An P256 public key hash"
    let size = BLAKE2b.BLAKE2b_160.digest_size
    let prefix = Base58.Prefix.p256_public_key_hash
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

    let prefix = Prefix.p256_signature
    let to_raw signature = signature

    let of_raw string =
      match String.length string = size with
      | true -> Some string
      | false -> None
  end)

  include With_yojson_of_b58 (struct
    type t = signature

    let of_b58 = of_b58
    let to_b58 = to_b58
  end)

  include BLAKE2b.With_alg (struct
    type secret = Secret.t
    type key = Key.t
    type signature = t

    let sign ?(prefix = "") secret hash =
      let prefix_and_hash = Cstruct.of_string (prefix ^ hash) in
      (* TODO: explain r and s*)
      let r, s = sign ~key:secret prefix_and_hash in
      let signature = Cstruct.append r s in
      Cstruct.to_string signature

    let verify ?(prefix = "") key signature hash =
      let r, s = (String.sub signature 0 32, String.sub signature 32 32) in
      let r, s = (Cstruct.of_string r, Cstruct.of_string s) in
      let prefix_and_hash = Cstruct.of_string (prefix ^ hash) in
      verify ~key (r, s) prefix_and_hash
  end)
end
