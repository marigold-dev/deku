open Helpers;

module Wallet_map = Map_with_yojson_make(Wallet);
[@deriving yojson]
type t = {free: Wallet_map.t(Amount.t)};

let empty = {free: Wallet_map.empty};

let balance = (address, t) =>
  Wallet_map.find_opt(address, t.free) |> Option.value(~default=Amount.zero);

let assert_available = (~source, ~amount: Amount.t) =>
  if (source >= amount) {
    Ok();
  } else {
    Error(`Not_enough_funds);
  };

let transfer = (~source, ~destination, amount, t) => {
  open Amount;

  let source_balance = balance(source, t);
  let.ok () = assert_available(~source=source_balance, ~amount);

  let destination_balance = balance(destination, t);

  Ok({
    free:
      t.free
      |> Wallet_map.add(source, source_balance - amount)
      |> Wallet_map.add(destination, destination_balance + amount),
  });
};

// tezos operations
let deposit = (destination, amount, t) => {
  open Amount;
  let destination_balance = balance(destination, t);
  {
    free: t.free |> Wallet_map.add(destination, destination_balance + amount),
  };
};
