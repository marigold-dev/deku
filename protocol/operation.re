open Helpers;
open Crypto;

module Main_chain = {
  [@deriving yojson]
  type kind =
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Deposit({
        destination: Address.t,
        amount: Amount.t,
        ticket: Ticket.t,
      });
  [@deriving yojson]
  type t = {
    hash: BLAKE2B.t,
    tezos_hash: BLAKE2B.t,
    tezos_index: int,
    kind,
  };
  let compare = (a, b) => BLAKE2B.compare(a.hash, b.hash);

  let (hash, verify) = {
    /* TODO: this is bad name, it exists like this to prevent
       duplicating all this name parameters */
    let apply = (f, ~tezos_hash, ~tezos_index, ~kind) => {
      let to_yojson = [%to_yojson: (BLAKE2B.t, int, kind)];
      let json = to_yojson((tezos_hash, tezos_index, kind));
      let payload = Yojson.Safe.to_string(json);
      f(payload);
    };
    let hash = apply(BLAKE2B.hash);
    let verify = (~hash) => apply(BLAKE2B.verify(~hash));
    (hash, verify);
  };

  let make = (~tezos_hash, ~tezos_index, ~kind) => {
    let hash = hash(~tezos_hash, ~tezos_index, ~kind);
    {hash, tezos_hash, tezos_index, kind};
  };
  let verify = (~hash, ~tezos_hash, ~tezos_index, ~kind) => {
    let.ok () =
      verify(~hash, ~tezos_hash, ~tezos_index, ~kind)
        ? Ok() : Error("Main operation invalid hash");
    Ok({hash, tezos_hash, tezos_index, kind});
  };

  let of_yojson = json => {
    let.ok {hash, tezos_hash, tezos_index, kind} = of_yojson(json);
    verify(~hash, ~tezos_hash, ~tezos_index, ~kind);
  };
};

module Side_chain = {
  // TODO: I don't like this structure model
  [@deriving yojson]
  type kind =
    | Transaction({
        destination: Address.t,
        amount: Amount.t,
        ticket: Ticket.t,
      })
    | Withdraw({
        owner: Tezos_interop.Address.t,
        amount: Amount.t,
        ticket: Ticket.t,
      })
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);

  [@deriving yojson]
  type t = {
    hash: BLAKE2B.t,
    signature: Protocol_signature.t,
    nonce: int32,
    block_height: int64,
    source: Address.t,
    kind,
  };
  let compare = (a, b) => BLAKE2B.compare(a.hash, b.hash);

  let (hash, verify) = {
    /* TODO: this is bad name, it exists like this to prevent
       duplicating all this name parameters */
    let apply = (f, ~nonce, ~block_height, ~source, ~kind) => {
      let to_yojson = [%to_yojson: (int32, int64, Address.t, kind)];
      let json = to_yojson((nonce, block_height, source, kind));
      let payload = Yojson.Safe.to_string(json);
      f(payload);
    };
    let hash = apply(BLAKE2B.hash);
    let verify = (~hash) => apply(BLAKE2B.verify(~hash));
    (hash, verify);
  };

  let verify = (~hash, ~signature, ~nonce, ~block_height, ~source, ~kind) => {
    let.ok () =
      verify(~hash, ~nonce, ~block_height, ~source, ~kind)
        ? Ok() : Error("Side operation invalid hash");
    let.ok () =
      Protocol_signature.verify(~signature, hash)
      && Address.pubkey_matches_wallet(
           Protocol_signature.public_key(signature),
           source,
         )
        ? Ok() : Error("Side operation invalid signature");
    Ok({hash, signature, nonce, block_height, source, kind});
  };

  let sign = (~secret, ~nonce, ~block_height, ~source, ~kind) => {
    let hash = hash(~nonce, ~block_height, ~source, ~kind);
    let signature = Protocol_signature.sign(~key=secret, hash);
    {hash, signature, nonce, block_height, source, kind};
  };

  let of_yojson = json => {
    let.ok {hash, signature, nonce, block_height, source, kind} =
      of_yojson(json);
    verify(~hash, ~signature, ~nonce, ~block_height, ~source, ~kind);
  };
};
