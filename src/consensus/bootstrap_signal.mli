open Deku_crypto

exception Invalid_signature

type bootstrap_signal =
  | Bootstrap_signal of {
      bootstrap_key : Key.key;
      signature : Signature.signature;
      next_author : Key_hash.key_hash;
    }

and t = bootstrap_signal [@@deriving yojson]
