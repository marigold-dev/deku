open Helpers;

module Main_chain = {
  [@deriving (ord, yojson)]
  type t =
    // TODO: can a validator uses the same key in different nodes?
    // If so the ordering in the list must never use the same key two times in sequence
    | Add_validator(Validators.validator)
    | Remove_validator(Validators.validator);
};

module Side_chain = {
  // TODO: I don't like this structure model
  [@deriving (ord, yojson)]
  type kind =
    | Transaction({destination: Wallet.t});
  [@deriving (ord, yojson)]
  type t = {
    hash: BLAKE2B.t,
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

  let make = (~nonce, ~block_height, ~source, ~amount, ~kind) => {
    let hash = hash(~nonce, ~block_height, ~source, ~amount, ~kind);
    {hash, nonce, block_height, source, amount, kind};
  };

  let of_yojson = json => {
    let.ok {hash, nonce, block_height, source, kind, amount} =
      of_yojson(json);
    let.ok () =
      verify(~hash, ~nonce, ~block_height, ~source, ~amount, ~kind)
        ? Ok() : Error("Invalid hash");
    Ok({hash, nonce, block_height, source, amount, kind});
  };

  // TODO: maybe use GADT for this?
  module Self_signed =
    Signed.Make({
      type nonrec t = t;
      let compare = compare;
      let to_yojson = to_yojson;
      let of_yojson = of_yojson;
      let verify = (~key, ~signature as _, data) =>
        Wallet.of_address(key) == data.source;
    });
};

let self_sign_side = (~key, op) => {
  let Signed.{key, signature, data} = Signed.sign(~key, op);
  Side_chain.Self_signed.verify(~key, ~signature, data) |> Result.get_ok;
};
