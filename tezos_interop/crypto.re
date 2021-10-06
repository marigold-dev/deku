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
module P256 = {
  module Dsa = P256.Dsa;
  module Compress = {
    let compress = (repr, t) => {
      let res = Cstruct.create(33);
      let b = Dsa.pub_to_cstruct(t);
      let ident = 2 + Cstruct.get_uint8(b, 63) land 1;
      Cstruct.blit(b, 1, res, 1, 32);
      Cstruct.set_uint8(res, 0, ident);
      repr(res);
    };
    let compress_to_string = compress(Cstruct.to_string);
    let compress_to_bytes = compress(Cstruct.to_bytes);
    let p =
      Z.of_string(
        "115792089210356248762697446949407573530086143415290314195533631308867097853951",
      ); // prime of secp256r1
    // domain parameter b of the curve
    let b =
      Z.of_string_base(
        16,
        "5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B",
      );
    // https://tools.ietf.org/id/draft-jivsov-ecc-compact-00.xml#sqrt point 4.3 precomputed
    let p_identity = Z.((p + one) /< of_int(4));
    let three = Z.of_int(3);
    let decompress = (repr, string) => {
      let buf = repr(string);
      let res = Cstruct.create(65);
      let x =
        Cstruct.sub(buf, 1, 32)
        |> Cstruct.rev  // zarith uses little endian for bits
        |> Cstruct.to_string
        |> Z.of_bits;
      let y = {
        open Z;
        // https://tools.ietf.org/id/draft-jivsov-ecc-compact-00.xml#sqrt point 4.3
        let interm = powm(x ** 3 - three * x + b, p_identity, p);
        let y = min(interm, p - interm);
        Z.to_bits(y) |> Cstruct.of_string |> Cstruct.rev;
      };
      Cstruct.set_uint8(res, 0, 4);
      Cstruct.blit(buf, 1, res, 1, 32);
      Cstruct.blit(y, 0, res, 33, 32);
      res |> Dsa.pub_of_cstruct |> Result.to_option;
    };
    let decompress_of_string = decompress(Cstruct.of_string);
    let decompress_of_bytes = decompress(Cstruct.of_bytes);
  };
  module Key = {
    type t = Dsa.pub_;

    let size = 33;
    let prefix = Base58.Prefix.p256_public_key;
    let equal = (a, b) => {
      let (a, b) = (Dsa.pub_to_cstruct(a), Dsa.pub_to_cstruct(b));
      Cstruct.equal(a, b);
    };
    let encoding = {
      let to_bytes = t => Compress.compress_to_bytes(t);
      let of_bytes_exn = b => Compress.decompress_of_bytes(b) |> Option.get;
      Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(size)));
    };

    let to_raw = t => Compress.compress_to_string(t);

    let of_raw = string => Compress.decompress_of_string(string);

    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
  module Key_hash = {
    [@deriving eq]
    type t = BLAKE2B_20.t;

    let hash_key = t => BLAKE2B_20.hash(Compress.compress_to_string(t));

    let encoding = {
      let name = "P256.Public_key_hash";
      Data_encoding.(obj1(req(name, blake2b_20_encoding)));
    };

    let prefix = Base58.Prefix.p256_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };

  module Secret = {
    type t = Dsa.priv;

    let equal = (a, b) => {
      let (a, b) = (Dsa.priv_to_cstruct(a), Dsa.priv_to_cstruct(b));
      Cstruct.equal(a, b);
    };
    let _size = 56;
    let prefix = Base58.Prefix.p256_secret_key;
    let to_raw = t => Cstruct.to_string(Dsa.priv_to_cstruct(t));
    let of_raw = string => {
      Dsa.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
    };

    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
  module Signature = {
    [@deriving eq]
    type t = string;
    let sign = (secret, message) => {
      let hash = BLAKE2B.hash(message);
      Cstruct.of_string(BLAKE2B.to_raw_string(hash))
      |> Dsa.sign(~key=secret)
      |> (((r, s)) => Cstruct.to_string(r) ++ Cstruct.to_string(s));
    };
    let check = (public, signature, message) => {
      let hash = BLAKE2B.hash(message);
      let (r, s) = (
        String.sub(signature, 0, 32),
        String.sub(signature, 32, 32),
      );
      Dsa.verify(
        ~key=public,
        (Cstruct.of_string(r), Cstruct.of_string(s)),
        Cstruct.of_string(BLAKE2B.to_raw_string(hash)),
      );
    };

    let size = 64;
    let prefix = Base58.Prefix.p256_signature;
    let to_raw = Fun.id;
    let of_raw = string =>
      String.length(string) == size ? Some(string) : None;
    let to_string = t => Base58.simple_encode(~prefix, ~to_raw, t);
    let of_string = string => Base58.simple_decode(~prefix, ~of_raw, string);
  };
};
