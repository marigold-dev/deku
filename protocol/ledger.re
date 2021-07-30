open Helpers;

module Wallet_and_ticket_map = {
  [@deriving (ord, yojson)]
  type key = {
    address: Wallet.t,
    ticket: Ticket.t,
  };
  module Map =
    Map_with_yojson_make({
      [@deriving (ord, yojson)]
      type t = key;
    });
  [@deriving yojson]
  type t = Map.t(Amount.t);
  let empty = Map.empty;
  let find_opt = (address, ticket) => Map.find_opt({address, ticket});
  let add = (address, ticket) => Map.add({address, ticket});
};
[@deriving yojson]
type t = {ledger: Wallet_and_ticket_map.t};

let empty = {ledger: Wallet_and_ticket_map.empty};

let balance = (address, ticket, t) =>
  Wallet_and_ticket_map.find_opt(address, ticket, t.ledger)
  |> Option.value(~default=Amount.zero);

let assert_available = (~source, ~amount: Amount.t) =>
  if (source >= amount) {
    Ok();
  } else {
    Error(`Not_enough_funds);
  };

let transfer = (~source, ~destination, amount, ticket, t) => {
  open Amount;

  let source_balance = balance(source, ticket, t);
  let.ok () = assert_available(~source=source_balance, ~amount);

  let destination_balance = balance(destination, ticket, t);

  Ok({
    ledger:
      t.ledger
      |> Wallet_and_ticket_map.add(source, ticket, source_balance - amount)
      |> Wallet_and_ticket_map.add(
           destination,
           ticket,
           destination_balance + amount,
         ),
  });
};

// tezos operations
let deposit = (destination, amount, ticket, t) => {
  open Amount;
  let destination_balance = balance(destination, ticket, t);
  {
    ledger:
      t.ledger
      |> Wallet_and_ticket_map.add(
           destination,
           ticket,
           destination_balance + amount,
         ),
  };
};
