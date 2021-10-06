open Helpers;
open Mirage_crypto_ec;
include Crypto_intf;
let blake2b_20_encoding =
  Data_encoding.(
    conv(
      hash => BLAKE2B_20.to_raw_string(hash) |> Bytes.of_string,
      // TODO: I don't like this exception below
      bytes =>
        Bytes.to_string(bytes) |> BLAKE2B_20.of_raw_string |> Option.get,
      Fixed.bytes(20),
    )
  );
module Ed25519 = {
  open Ed25519;
  module Key = {
    type t = pub_;

    let size = 32;
    let prefix = Base58.Prefix.ed25519_public_key;
    let equal = (a, b) => {
      let (a, b) = (pub_to_cstruct(a), pub_to_cstruct(b));
      Cstruct.equal(a, b);
    };
    let encoding = {
      // TODO: in tezos this is splitted json is not same as binary
      let to_bytes = t => pub_to_cstruct(t) |> Cstruct.to_bytes;
      let of_bytes_exn = b =>
        Cstruct.of_bytes(b) |> pub_of_cstruct |> Result.get_ok;
      Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(size)));
    };
    let to_raw = t => Cstruct.to_string(Ed25519.pub_to_cstruct(t));
    let of_raw = string =>
      Ed25519.pub_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };

  module Key_hash = {
    [@deriving eq]
    type t = BLAKE2B_20.t;

    let hash_key = t =>
      BLAKE2B_20.hash(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string);

    let encoding = {
      let name = "Ed25519.Public_key_hash";
      // TODO: in tezos this is splitted json is not same as bin
      Data_encoding.(obj1(req(name, blake2b_20_encoding)));
    };

    let prefix = Base58.Prefix.ed25519_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };

  module Secret = {
    type t = priv;
    let equal = (a, b) => {
      let (a, b) = (priv_to_cstruct(a), priv_to_cstruct(b));
      Cstruct.equal(a, b);
    };
    let _size = 32;
    let prefix = Base58.Prefix.ed25519_seed;
    let to_raw = t => Cstruct.to_string(Ed25519.priv_to_cstruct(t));
    let of_raw = string =>
      Ed25519.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;

    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
  module Signature = {
    [@deriving eq]
    type t = string;
    let sign = (secret, message) => {
      // double hash because tezos always uses blake2b on CHECK_SIGNATURE
      let hash = BLAKE2B.hash(message);
      Cstruct.of_string(BLAKE2B.to_raw_string(hash))
      // TODO: isn't this double hashing? Seems weird
      |> Ed25519.sign(~key=secret)
      |> Cstruct.to_string;
    };
    let check = (public, signature, message) => {
      let hash = BLAKE2B.hash(message);
      verify(
        ~key=public,
        ~msg=Cstruct.of_string(BLAKE2B.to_raw_string(hash)),
        Cstruct.of_string(signature),
      );
    };

    let size = 64;
    let prefix = Base58.Prefix.ed25519_signature;
    let to_raw = Fun.id;
    let of_raw = string =>
      String.length(string) == size ? Some(string) : None;
    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
};
module Secp256k1 = {
  open Libsecp256k1.External;
  module K = Key;
  let context = {
    Mirage_crypto_rng_unix.initialize(); //TODO: adding this to to test files doesnt help ??
    let c = Context.create();
    let rand_value =
      Mirage_crypto_rng.generate(32) |> Cstruct.to_bytes |> Bigstring.of_bytes;
    let randomized = Context.randomize(c, rand_value);
    randomized
      ? c : failwith("Secp256k1 context randomization failed. Aborting.");
  };

  module Key = {
    type t = K.t(K.public);
    let equal = (a, b) => {
      Bigstring.equal(K.to_bytes(context, a), K.to_bytes(context, b));
    };
    let size = K.compressed_pk_bytes;
    let prefix = Base58.Prefix.secp256k1_public_key;
    let encoding = {
      let to_bytes = t => K.to_bytes(context, t) |> Bigstring.to_bytes;
      let of_bytes_exn = b =>
        Key.read_pk_exn(context, Bigstring.of_bytes(b));
      Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(size)));
    };

    let to_raw = t => Key.to_bytes(context, t) |> Bigstring.to_string;

    let of_raw = string =>
      Key.read_pk(context, string |> Bigstring.of_string) |> Result.to_option;

    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
  module Key_hash = {
    [@deriving eq]
    type t = BLAKE2B_20.t;

    let hash_key = t => BLAKE2B_20.hash(Key.to_raw(t));

    let encoding = {
      let name = "Secp256k1.Public_key_hash";
      Data_encoding.(obj1(req(name, blake2b_20_encoding)));
    };

    let prefix = Base58.Prefix.secp256k1_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
  module Secret = {
    type t = K.t(K.secret);

    let equal = (a, b) => {
      Bigstring.equal(K.to_bytes(context, a), K.to_bytes(context, b));
    };
    let _size = K.secret_bytes;
    let prefix = Base58.Prefix.secp256k1_secret_key;
    let to_raw = t => K.to_bytes(context, t) |> Bigstring.to_string;
    let of_raw = string => {
      K.read_sk(context, string |> Bigstring.of_string) |> Result.to_option;
    };

    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
  module Signature = {
    [@deriving eq]
    type t = string;
    let sign = (secret, message) => {
      let hash = BLAKE2B.hash(message);
      Bigstring.of_string(BLAKE2B.to_raw_string(hash))
      |> Sign.sign_exn(context, ~sk=secret)
      |> Sign.to_bytes(~der=false, context)
      |> Bigstring.to_string;
    };
    let check = (public, signature, message) => {
      let hash = BLAKE2B.hash(message);
      let signature = Sign.read_exn(context, Bigstring.of_string(signature));
      Sign.verify_exn(
        context,
        ~pk=public,
        ~msg=Bigstring.of_string(BLAKE2B.to_raw_string(hash)),
        ~signature,
      );
    };

    let size = Sign.plain_bytes;
    let prefix = Base58.Prefix.secp256k1_signature;
    let to_raw = Fun.id;
    let of_raw = string =>
      String.length(string) == size ? Some(string) : None;
    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
};
