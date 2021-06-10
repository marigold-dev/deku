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

  let sk_size = 32;
  let prefix = Base58.Prefix.ed25519_public_key;
  let encoding = {
    // TODO: in tezos this is splitted json is not same as binary
    let to_bytes = t => pub_to_cstruct(t) |> Cstruct.to_bytes;
    let of_bytes_exn = b =>
      Cstruct.of_bytes(b) |> pub_of_cstruct |> Result.get_ok;
    Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(sk_size)));
  };

  let to_raw = t => Cstruct.to_string(Ed25519.pub_to_cstruct(t));
  let of_raw = string =>
    Ed25519.pub_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
  let to_string = Base58.simple_encode(~prefix, ~to_raw);
  let of_string = Base58.simple_decode(~prefix, ~of_raw);
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
