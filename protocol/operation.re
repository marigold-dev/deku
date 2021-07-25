open Helpers;

module Main_chain = {
  [@deriving (ord, yojson)]
  type kind =
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);
  [@deriving yojson]
  type t = {
    tezos_hash: BLAKE2B.t,
    kind,
  };
  let compare = (a, b) => BLAKE2B.compare(a.tezos_hash, b.tezos_hash);

  let make = (~tezos_hash, ~kind) => {
    {tezos_hash, kind};
  };
};

module Side_chain = {
  // TODO: I don't like this structure model
  [@deriving (ord, yojson)]
  type kind =
    | Transaction({destination: Wallet.t});
  [@deriving (ord, yojson)]
  type t = {
    hash: BLAKE2B.t,
    signature: Signature.t,
    nonce: int32,
    block_height: int64,
    source: Wallet.t,
    amount: Amount.t,
    kind,
  };

  let (hash, verify) = {
    /* TODO: this is bad name, it exists like this to prevent
       duplicating all this name parameters */
    let apply = (f, ~nonce, ~block_height, ~source, ~kind, ~amount) => {
      let to_yojson = [%to_yojson: (int32, int64, Wallet.t, Amount.t, kind)];
      let json = to_yojson((nonce, block_height, source, amount, kind));
      let payload = Yojson.Safe.to_string(json);
      f(payload);
    };
    let hash = apply(BLAKE2B.hash);
    let verify = (~hash) => apply(BLAKE2B.verify(~hash));
    (hash, verify);
  };

  let verify =
      (~hash, ~signature, ~nonce, ~block_height, ~source, ~amount, ~kind) => {
    let.ok () =
      verify(~hash, ~nonce, ~block_height, ~source, ~amount, ~kind)
        ? Ok() : Error("Side operation invalid hash");
    let.ok () =
      Signature.verify(~signature, hash)
      && Wallet.pubkey_matches_wallet(
           Signature.public_key(signature),
           source,
         )
        ? Ok() : Error("Side operation invalid signature");
    Ok({hash, signature, nonce, block_height, source, amount, kind});
  };

  let sign = (~secret, ~nonce, ~block_height, ~source, ~amount, ~kind) => {
    let hash = hash(~nonce, ~block_height, ~source, ~amount, ~kind);
    let signature = Signature.sign(~key=secret, hash);
    {hash, signature, nonce, block_height, source, amount, kind};
  };

  let of_yojson = json => {
    let.ok {hash, signature, nonce, block_height, source, kind, amount} =
      of_yojson(json);
    verify(~hash, ~signature, ~nonce, ~block_height, ~source, ~kind, ~amount);
  };
};
