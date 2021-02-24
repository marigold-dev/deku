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
      });
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
    nonce: int32,
    block_height: int64,
    source: Wallet.t,
    amount: Amount.t,
    kind,
  };

  module Set =
    Set.Make({
      type nonrec t = t;
      let compare = compare;
    });
  // TODO: Signed that ensures source
};
