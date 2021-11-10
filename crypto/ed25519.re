open Mirage_crypto_ec;
open Ed25519;
open Helpers;

module Secret = {
  type t = priv;
  let equal = (a, b) => {
    let (a, b) = (priv_to_cstruct(a), priv_to_cstruct(b));
    Cstruct.equal(a, b);
  };
  let compare = (a, b) =>
    Cstruct.compare(priv_to_cstruct(a), priv_to_cstruct(b));

  include Encoding_helpers.Make_b58({
    type nonrec t = t;
    let name = "Ed25519.Secret_key";
    let title = "An Ed25519 secret key";

    let size = 32;
    let prefix = Base58.Prefix.ed25519_seed;

    let to_raw = t => Cstruct.to_string(Ed25519.priv_to_cstruct(t));
    let of_raw = string =>
      Ed25519.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
  });
};

module Key = {
  type t = pub_;

  let of_secret = Ed25519.pub_of_priv;
  let equal = (a, b) => {
    let (a, b) = (pub_to_cstruct(a), pub_to_cstruct(b));
    Cstruct.equal(a, b);
  };
  let compare = (a, b) =>
    Cstruct.compare(pub_to_cstruct(a), pub_to_cstruct(b));

  include Encoding_helpers.Make_b58({
    type nonrec t = t;
    let name = "Ed25519.Public_key";
    let title = "Ed25519 public key";

    let size = 32;
    let prefix = Base58.Prefix.ed25519_public_key;

    let to_raw = t => Cstruct.to_string(Ed25519.pub_to_cstruct(t));
    let of_raw = string =>
      Ed25519.pub_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
  });
};

module Key_hash = {
  [@deriving (ord, eq)]
  type t = BLAKE2B_20.t;

  let of_key = t =>
    BLAKE2B_20.hash(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string);

  let encoding = {
    let name = "Ed25519.Public_key_hash";
    // TODO: in tezos this is splitted json is not same as bin
    Data_encoding.(obj1(req(name, BLAKE2B_20.encoding)));
  };

  let prefix = Base58.Prefix.ed25519_public_key_hash;
  let to_raw = BLAKE2B_20.to_raw_string;
  let of_raw = BLAKE2B_20.of_raw_string;
  let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
  let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
};

module Signature = {
  [@deriving (ord, eq)]
  type t = string;

  include Encoding_helpers.Make_b58({
    type nonrec t = t;
    let name = "Ed25519";
    let title = "An Ed25519 signature";

    let size = 64;
    let prefix = Base58.Prefix.ed25519_signature;

    let to_raw = Fun.id;
    let of_raw = string =>
      String.length(string) == size ? Some(string) : None;
  });
};

let sign = (secret, hash) =>
  Cstruct.of_string(BLAKE2B.to_raw_string(hash))
  |> Ed25519.sign(~key=secret)
  |> Cstruct.to_string;
let verify = (public, signature, hash) =>
  verify(
    ~key=public,
    ~msg=Cstruct.of_string(BLAKE2B.to_raw_string(hash)),
    Cstruct.of_string(signature),
  );
let generate = () => Ed25519.generate();
