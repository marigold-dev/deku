open Helpers;

module Main_chain = {
  [@deriving yojson]
  type kind =
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Deposit({
        destination: Wallet.t,
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
    | Transaction({destination: Wallet.t})
    | Withdraw({owner: Tezos_interop.Address.t})
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);

  [@deriving yojson]
  type t = {
    hash: BLAKE2B.t,
    signature: Signature.t,
    nonce: int32,
    block_height: int64,
    source: Wallet.t,
    amount: Amount.t,
    ticket: Ticket.t,
    kind,
  };
  let compare = (a, b) => BLAKE2B.compare(a.hash, b.hash);

  let (hash, verify) = {
    /* TODO: this is bad name, it exists like this to prevent
       duplicating all this name parameters */
    let apply = (f, ~nonce, ~block_height, ~source, ~amount, ~ticket, ~kind) => {
      let to_yojson = [%to_yojson:
        (int32, int64, Wallet.t, Amount.t, Ticket.t, kind)
      ];
      let json =
        to_yojson((nonce, block_height, source, amount, ticket, kind));
      let payload = Yojson.Safe.to_string(json);
      f(payload);
    };
    let hash = apply(BLAKE2B.hash);
    let verify = (~hash) => apply(BLAKE2B.verify(~hash));
    (hash, verify);
  };

  let verify =
      (
        ~hash,
        ~signature,
        ~nonce,
        ~block_height,
        ~source,
        ~amount,
        ~ticket,
        ~kind,
      ) => {
    let.ok () =
      verify(~hash, ~nonce, ~block_height, ~source, ~amount, ~ticket, ~kind)
        ? Ok() : Error("Side operation invalid hash");
    let.ok () =
      Signature.verify(~signature, hash)
      && Wallet.pubkey_matches_wallet(
           Signature.public_key(signature),
           source,
         )
        ? Ok() : Error("Side operation invalid signature");
    Ok({hash, signature, nonce, block_height, source, amount, ticket, kind});
  };

  let sign =
      (~secret, ~nonce, ~block_height, ~source, ~amount, ~ticket, ~kind) => {
    let hash = hash(~nonce, ~block_height, ~source, ~amount, ~ticket, ~kind);
    let signature = Signature.sign(~key=secret, hash);
    {hash, signature, nonce, block_height, source, amount, ticket, kind};
  };

  let of_yojson = json => {
    let.ok {
      hash,
      signature,
      nonce,
      block_height,
      source,
      amount,
      ticket,
      kind,
    } =
      of_yojson(json);
    verify(
      ~hash,
      ~signature,
      ~nonce,
      ~block_height,
      ~source,
      ~kind,
      ~ticket,
      ~amount,
    );
  };
};
