open Helpers;
open Crypto;
open Core;

module Consensus = {
  [@deriving (eq, ord, yojson)]
  type t =
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);

  let hash = payload =>
    to_yojson(payload) |> Yojson.Safe.to_string |> BLAKE2B.hash;
  let sign = (secret, t) => {
    let hash = hash(t);
    Signature.sign(secret, hash);
  };
  let verify = (key, signature, t) => {
    let hash = hash(t);
    Signature.verify(key, signature, hash);
  };
};
module Core_tezos = {
  [@deriving (eq, ord, yojson)]
  type t = Tezos_operation.t;
};
module Core_user = {
  [@deriving (eq, yojson)]
  type t = {
    // signature
    hash: BLAKE2B.t,
    key: Key.t,
    signature: Signature.t,
    // replay
    nonce: int32,
    block_height: int64,
    data: User_operation.t,
  };
  let compare = (a, b) => BLAKE2B.compare(a.hash, b.hash);

  let (hash, verify) = {
    /* TODO: this is bad name, it exists like this to prevent
       duplicating all this name parameters */
    let apply = (f, ~nonce, ~block_height, ~data) => {
      // TODO: header to distinguish hashes
      let to_yojson = [%to_yojson: (int32, int64, User_operation.t)];
      let json = to_yojson((nonce, block_height, data));
      let payload = Yojson.Safe.to_string(json);
      f(payload);
    };
    let hash = apply(BLAKE2B.hash);
    let verify = (~hash) => apply(BLAKE2B.verify(~hash));
    (hash, verify);
  };

  let sign = (~secret, ~nonce, ~block_height, ~data) => {
    let hash = hash(~nonce, ~block_height, ~data);
    let key = Key.of_secret(secret);
    let signature = Signature.sign(secret, hash);
    // TODO: this can only happen through a bug
    //       maybe the API should enforce it
    assert(Address.matches_key(key, data.source));
    {hash, key, signature, nonce, block_height, data};
  };

  let verify = (~hash, ~key, ~signature, ~nonce, ~block_height, ~data) => {
    let.assert () = (
      "Invalid core_user operation hash",
      verify(~hash, ~nonce, ~block_height, ~data),
    );
    let.assert () = (
      "Invalid core_user key",
      Address.matches_key(key, data.source),
    );
    let.assert () = (
      "Invalid core_user signature",
      Signature.verify(key, signature, hash),
    );
    Ok({hash, key, signature, nonce, block_height, data});
  };
  let of_yojson = json => {
    let.ok {hash, key, signature, nonce, block_height, data} =
      of_yojson(json);
    verify(~hash, ~key, ~signature, ~nonce, ~block_height, ~data);
  };

  let unsafe_make = (~hash, ~key, ~signature, ~nonce, ~block_height, ~data) => {
    hash,
    key,
    signature,
    nonce,
    block_height,
    data,
  };
};

[@deriving (eq, ord, yojson)]
type t =
  | Core_tezos(Core.Tezos_operation.t)
  | Core_user(Core_user.t)
  | Consensus(Consensus.t);
