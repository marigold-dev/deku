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
module Handle = {
  [@deriving yojson]
  type t = {
    hash: BLAKE2B.t,
    id: int,
    owner: Tezos_interop.Address.t,
    amount: Amount.t,
    ticket: Ticket.t,
  };
  let hash = (~id, ~owner, ~amount, ~ticket) => {
    let Ticket.{ticketer, data} = ticket;
    Tezos_interop.Consensus.hash_withdraw_handle(
      ~id=Z.of_int(id),
      ~owner,
      ~amount=Z.of_int(Amount.to_int(amount)),
      ~ticketer,
      ~data,
    );
  };
};
module Handle_tree =
  Incremental_patricia.Make({
    [@deriving yojson]
    type t = Handle.t;
    let hash = t => t.Handle.hash;
  });
[@deriving yojson]
type t = {
  ledger: Wallet_and_ticket_map.t,
  handles: Handle_tree.t,
};

let empty = {ledger: Wallet_and_ticket_map.empty, handles: Handle_tree.empty};

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
    handles: t.handles,
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
    handles: t.handles,
  };
};
let withdraw = (~source, ~destination, amount, ticket, t) => {
  open Amount;
  let owner = destination;
  let source_balance = balance(source, ticket, t);
  let.ok () = assert_available(~source=source_balance, ~amount);

  let (handles, handle) =
    Handle_tree.add(
      id => {
        let hash = Handle.hash(~id, ~owner, ~amount, ~ticket);
        {id, hash, owner, amount, ticket};
      },
      t.handles,
    );
  let t = {
    ledger:
      t.ledger
      |> Wallet_and_ticket_map.add(source, ticket, source_balance - amount),
    handles,
  };
  Ok((t, handle));
};

let handles_find_proof = (key, t) => Handle_tree.find(key, t.handles);
let handles_root_hash = t => Handle_tree.hash(t.handles);
