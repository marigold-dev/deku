open Helpers;
open Exn_noop;

module Address_map = Map_with_yojson_make(Address);
[@deriving yojson]
type t = {
  free: Address_map.t(Amount.t),
  frozen: Address_map.t(Amount.t),
};

let empty = {free: Address_map.empty, frozen: Address_map.empty};

let get = (address, map) =>
  Address_map.find_opt(address, map) |> Option.value(~default=Amount.zero);

let get_free = (address, t) => get(address, t.free);
let get_frozen = (address, t) => get(address, t.frozen);

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
      |> Address_map.add(source, source_balance - amount)
      |> Address_map.add(destination, destination_balance + amount),
    frozen: t.frozen,
  };
};

let freeze = (~address, ~amount, t) => {
  open Amount;

  let source_balance = get_free(address, t);
  assert_available(~source=source_balance, ~amount);

  let destination_balance = get_frozen(address, t);
  {
    free: t.free |> Address_map.add(address, source_balance - amount),
    frozen:
      t.frozen |> Address_map.add(address, destination_balance + amount),
  };
};
// TODO: avoid this duplicated code
let unfreeze = (~address, ~amount, t) => {
  open Amount;

  let source_balance = get_frozen(address, t);
  assert_available(~source=source_balance, ~amount);

  let destination_balance = get_free(address, t);

  {
    free: t.free |> Address_map.add(address, destination_balance + amount),
    frozen: t.frozen |> Address_map.add(address, source_balance - amount),
  };
};

// tezos operations
let deposit = (~destination, ~amount, t) => {
  open Amount;
  let destination_balance = get_frozen(destination, t);
  {
    free: t.free,
    frozen:
      t.frozen |> Address_map.add(destination, destination_balance + amount),
  };
};
let withdraw = (~source, ~amount, t) => {
  open Amount;
  let source_balance = get_frozen(source, t);
  assert_available(~source=source_balance, ~amount);

  {
    free: t.free,
    frozen: t.frozen |> Address_map.add(source, source_balance - amount),
  };
};
