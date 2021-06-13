open Helpers;
open Talk_tezos;
open Mirage_crypto_ec;

let rec try_decode_list = (l, string) =>
  switch (l) {
  | [of_string, ...l] =>
    switch (of_string(string)) {
    | Some(v) => Some(v)
    | None => try_decode_list(l, string)
    }
  | [] => None
  };

module Ed25519 = {
  open Ed25519;

  type t = pub_;

  let size = 32;
  let prefix = Base58.Prefix.ed25519_public_key;
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
  let to_string = Base58.simple_encode(~prefix, ~to_raw);
  let of_string = Base58.simple_decode(~prefix, ~of_raw);

  module Hash = {
    type t = BLAKE2B_20.t;

    let hash_key = t =>
      BLAKE2B_20.hash(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string);

    // TODO: encoding

    let prefix = Base58.Prefix.ed25519_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };

  module Secret = {
    type t = priv;

    let _size = 32;
    let prefix = Base58.Prefix.ed25519_seed;
    let to_raw = t => Cstruct.to_string(Ed25519.priv_to_cstruct(t));
    let of_raw = string =>
      Ed25519.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;

    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
  module Signature = {
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
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
};

module Key = {
  type t =
    | Ed25519(Ed25519.t);

  let name = "Signature.Public_key";
  let title = "A Ed25519, Secp256k1, or P256 public key";
  let encoding = {
    open Data_encoding;
    let raw_encoding =
      def("public_key", ~description=title) @@
      union([
        case(
          Tag(0),
          Ed25519.encoding,
          ~title="Ed25519",
          fun
          | Ed25519(x) => Some(x),
          x =>
          Ed25519(x)
        ),
      ]);

    // TODO: move this to a functor
    obj1(req(name, raw_encoding));
  };
  let to_string =
    fun
    | Ed25519(key) => Ed25519.to_string(key);
  let of_string = {
    let ed25519 = string => {
      let.some key = Ed25519.of_string(string);
      Some(Ed25519(key));
    };
    try_decode_list([ed25519]);
  };
};

module Key_hash = {
  type t =
    | Ed25519(Ed25519.Hash.t);

  let of_key = t =>
    switch (t) {
    | Key.Ed25519(pub_) => Ed25519(Ed25519.Hash.hash_key(pub_))
    };

  let to_string =
    fun
    | Ed25519(hash) => Ed25519.Hash.to_string(hash);
  let of_string = {
    let ed25519 = string => {
      let.some key = Ed25519.Hash.of_string(string);
      Some(Ed25519(key));
    };
    try_decode_list([ed25519]);
  };
};
module Secret = {
  type t =
    | Ed25519(Ed25519.Secret.t);

  let to_string =
    fun
    | Ed25519(secret) => Ed25519.Secret.to_string(secret);
  let of_string = {
    let ed25519 = string => {
      let.some secret = Ed25519.Secret.of_string(string);
      Some(Ed25519(secret));
    };
    try_decode_list([ed25519]);
  };
};
module Signature = {
  type t =
    | Ed25519(Ed25519.Signature.t);

  let sign = (secret, message) =>
    switch (secret) {
    | Secret.Ed25519(secret) =>
      Ed25519(Ed25519.Signature.sign(secret, message))
    };
  let check = (key, signature, message) =>
    switch (key, signature) {
    | (Key.Ed25519(key), Ed25519(signature)) =>
      Ed25519.Signature.check(key, signature, message)
    };
  let to_string =
    fun
    | Ed25519(sign) => Ed25519.Signature.to_string(sign);
  let of_string = {
    let ed25519 = string => {
      let.some sign = Ed25519.Signature.of_string(string);
      Some(Ed25519(sign));
    };
    try_decode_list([ed25519]);
  };
};

module Pack = {
  open Talk_tezos;
  open Tezos_micheline;
  open Micheline;
  open Michelson_v1_primitives;

  type t = node(canonical_location, prim);

  let int = n => Int(-1, n);
  let bytes = b => Bytes(-1, b);
  let pair = (l, r) => Prim(-1, D_Pair, [l, r], []);
  let list = l => Seq(-1, l);
  let key = k =>
    Bytes(-1, Data_encoding.Binary.to_bytes_exn(Key.encoding, k));

  let expr_encoding =
    Micheline.canonical_encoding_v1(
      ~variant="michelson_v1",
      Michelson_v1_primitives.prim_encoding,
    );
  let to_bytes = data =>
    Data_encoding.Binary.to_bytes_exn(expr_encoding, strip_locations(data))
    |> Bytes.cat(Bytes.of_string("\005"));
};
