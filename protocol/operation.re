open Helpers;

module Main_chain = {
  [@deriving (ord, yojson)]
  type t =
    | Deposit({
        destination: Wallet.t,
        amount: Amount.t,
      })
    | Withdraw({
        source: Wallet.t,
        amount: Amount.t,
      })
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);
};

module Side_chain = {
  // TODO: I don't like this structure model
  [@deriving (ord, yojson)]
  type kind =
    | Transaction({destination: Wallet.t})
    | Freeze
    | Unfreeze;
  [@deriving (ord, yojson)]
  type t = {
    hash: SHA256.hash,
    nonce: int32,
    block_height: int64,
    source: Wallet.t,
    amount: Amount.t,
    kind,
  };

  let make = (~nonce, ~block_height, ~source, ~amount, ~kind) => {
    let SHA256.Magic.{hash, _} =
      SHA256.Magic.hash((nonce, block_height, source, amount, kind));
    {hash, nonce, block_height, source, kind, amount};
  };

  let of_yojson = json => {
    let.ok {hash, nonce, block_height, source, kind, amount} =
      of_yojson(json);
    let.ok {hash, _} =
      SHA256.Magic.verify(
        ~hash,
        (nonce, block_height, source, kind, amount),
      );
    Ok({hash, nonce, block_height, source, kind, amount});
  };

  // TODO: maybe use GADT for this?
  module Self_signed =
    Signed.Make({
      type nonrec t = t;
      let compare = compare;
      let to_yojson = to_yojson;
      let of_yojson = of_yojson;
      let verify = (~key, ~signature as _, data) =>
        key == Wallet.get_address(data.source);
    });
};
