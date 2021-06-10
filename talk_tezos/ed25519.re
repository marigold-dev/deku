module Public_key = {
  open Mirage_crypto_ec.Ed25519;
  type t = pub_;
  let (to_b58check, of_b58check) = {
    let prefix = Base58.Prefix.ed25519_public_key_hash;
    let to_raw = t => Cstruct.to_string(pub_to_cstruct(t));
    let of_raw = str =>
      pub_of_cstruct(Cstruct.of_string(str)) |> Result.to_option;
    (
      Base58.simple_encode(~prefix, ~to_raw),
      Base58.simple_decode(~prefix, ~of_raw),
    );
  };

  let encoding = {
    open Data_encoding;
    // TODO: in tezos this is splitted json is not same as binary
    let ed25519_encoding = {
      let sk_size = 32;
      let to_bytes = t => pub_to_cstruct(t) |> Cstruct.to_bytes;
      let of_bytes_exn = b =>
        Cstruct.of_bytes(b) |> pub_of_cstruct |> Result.get_ok;
      Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(sk_size)));
    };
    let name = "Signature.Public_key";
    let title = "A Ed25519 public key";
    let raw_encoding =
      def("public_key", ~description=title) @@
      union([
        case(
          Tag(0),
          ed25519_encoding,
          ~title="Ed25519",
          x => Some(x),
          x => x,
        ),
      ]);
    obj1(req(name, raw_encoding));
  };
  let to_tezos_bytes = b => Data_encoding.Binary.to_bytes_exn(encoding, b);
};

module Public_key_hash = {
  open Mirage_crypto_ec.Ed25519;
  type t = pub_;

  let (to_b58check, of_b58check) = {
    let prefix = Base58.Prefix.ed25519_public_key_hash;
    let to_raw = t => Cstruct.to_string(pub_to_cstruct(t));
    let of_raw = str =>
      pub_of_cstruct(Cstruct.of_string(str)) |> Result.to_option;
    (
      Base58.simple_encode(~prefix, ~to_raw),
      Base58.simple_decode(~prefix, ~of_raw),
    );
  };
};
