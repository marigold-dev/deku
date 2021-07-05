open Helpers;
open Exn_noop;

module Wallet_map = Map_with_yojson_make(Wallet);
[@deriving yojson]
type t = {free: Wallet_map.t(Amount.t)};

let empty = {free: Wallet_map.empty};

let get = (address, map) =>
  Wallet_map.find_opt(address, map) |> Option.value(~default=Amount.zero);

let get_free = (address, t) => get(address, t.free);

let assert_available = (~source, ~amount: Amount.t) =>
  if (source < amount) {
    raise(Noop("not enough funds"));
  };
let transfer = (~source, ~destination, ~amount, t) => {
  open Amount;

  let source_balance = get_free(source, t);
  assert_available(~source=source_balance, ~amount);

  let destination_balance = get_free(destination, t);

  {
    free:
      t.free
      |> Wallet_map.add(source, source_balance - amount)
      |> Wallet_map.add(destination, destination_balance + amount),
  };
};

// tezos operations
let deposit = (~destination, ~amount, t) => {
  open Amount;
  let destination_balance = get_free(destination, t);
  {
    free: t.free |> Wallet_map.add(destination, destination_balance + amount),
  };
};
