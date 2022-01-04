open Helpers;
open Crypto;

module Implicit_address_and_ticket_map = {
  [@deriving (ord, yojson)]
  type key = {
    address: Address.Implicit.t,
    ticket: Ticket.t,
  };
  module Map =
    Map.Make_with_yojson({
      [@deriving (ord, yojson)]
      type t = key;
    });
  [@deriving yojson]
  type t = Map.t(Ticket_table.Handle.t);
  let empty = Map.empty;
  let find_opt = (address, ticket) => Map.find_opt({address, ticket});
  let add = (address, ticket) => Map.add({address, ticket});
};
module Withdrawal_handle = {
  [@deriving yojson]
  type t = {
    hash: BLAKE2B.t,
    id: int,
    owner: Tezos.Address.t,
    amount: Amount.t,
    ticket: Ticket_id.t,
  };
  let hash = (~id, ~owner, ~amount, ~ticket) => {
    let Ticket_id.{ticketer, data} = ticket;
    Tezos.Deku.Consensus.hash_withdraw_handle(
      ~id=Z.of_int(id),
      ~owner,
      ~amount=Z.of_int(Amount.to_int(amount)),
      ~ticketer,
      ~data,
    );
  };
};
module Withdrawal_handle_tree =
  Incremental_patricia.Make({
    [@deriving yojson]
    type t = Withdrawal_handle.t;
    let hash = t => t.Withdrawal_handle.hash;
  });
[@deriving yojson]
type t = {
  ledger: Implicit_address_and_ticket_map.t,
  handles: Handle_tree.t,
};

let empty = {
  ledger: Implicit_address_and_ticket_map.empty,
  handles: Handle_tree.empty,
};

let balance = (address, ticket, t) =>
  Implicit_address_and_ticket_map.find_opt(address, ticket, t.ledger)
  |> Option.value(~default=Amount.zero);

let assert_available = (~source, ~amount: Amount.t) =>
  if (source >= amount) {
    Ok();
  } else {
    Error(`Not_enough_funds);
  };
  balance |> Option.value(~default=Amount.zero);
};

let get_or_create = (address, ticket, ticket_table, ledger) => {
  switch (Implicit_address_and_ticket_map.find_opt(address, ticket, ledger)) {
  | None =>
    let (handle, ticket_table) =
      Ticket_table.add_empty(ticket, ticket_table);
    let ledger =
      Implicit_address_and_ticket_map.add(address, ticket, handle, ledger);
    (handle, ticket_table, ledger);
  | Some(handle) => (handle, ticket_table, ledger)
  };
};

let transfer = (~source, ~destination, amount, ticket, t) => {
  let.ok source_handle =
    Implicit_address_and_ticket_map.find_opt(source, ticket, t.ledger)
    |> Option.to_result(~none=`Not_enough_funds);
  let (destination_handle, ticket_table, ledger) =
    get_or_create(destination, ticket, t.ticket_table, t.ledger);
  let.ok (Ticket_table.{split_at, remaining: source_handle}, ticket_table) =
    Ticket_table.split(source_handle, ~at=amount, ticket_table)
    |> Result.map_error(
         fun
         | `Invalid_ticket => failwith("Should be impossible")
         | `Not_enough_funds as e => e,
       );

  let (destination_handle, ticket_table) =
    Ticket_table.join(destination_handle, split_at, ticket_table)
    |> Result.get_ok;

  Ok({
    ledger:
      t.ledger
      |> Implicit_address_and_ticket_map.add(
           source,
           ticket,
           source_balance - amount,
         )
      |> Implicit_address_and_ticket_map.add(
           destination,
           ticket,
           destination_balance + amount,
         ),
    handles: t.handles,
  });
};

// tezos operations

let redeem_ticket_handle = (~destination, ~ticket_id=?, handle, t) => {
  let.ok ticket_id =
    switch (ticket_id) {
    | Some(id) => Ok(id)
    | None =>
      Ticket_table.get_id(handle, t.ticket_table)
      |> Option.to_result(~none=`Invalid_ticket)
    };
  let (destination_handle, ticket_table, ledger) =
    get_or_create(destination, ticket_id, t.ticket_table, t.ledger);
  let (destination_handle, ticket_table) =
    Ticket_table.join(destination_handle, handle, ticket_table)
    |> Result.get_ok;
  Ok({
    ledger:
      t.ledger
      |> Implicit_address_and_ticket_map.add(
           destination,
           ticket,
           destination_balance + amount,
         ),
    handles: t.handles,
  };
};

let deposit = (destination, amount, ticket_id, t) => {
  let (handle, ticket_table) =
    Ticket_table.add(
      Ticket_table.unsafe_create_ticket(ticket_id, amount),
      t.ticket_table,
    );
  redeem_ticket_handle(
    ~destination,
    ~ticket_id,
    handle,
    {...t, ticket_table},
  )
  |> Result.get_ok;
};

let withdraw = (~source, ~destination, amount, ticket, t) => {
  let owner = destination;

  let.ok source_handle =
    Implicit_address_and_ticket_map.find_opt(source, ticket, t.ledger)
    |> Option.to_result(~none=`Not_enough_funds);

  let.ok (Ticket_table.{split_at, remaining}, ticket_table) =
    Ticket_table.split(source_handle, ~at=amount, t.ticket_table)
    |> Result.map_error(
         fun
         | `Invalid_ticket => failwith("Should be impossible")
         | `Not_enough_funds as e => e,
       );

  let (Ticket_table.{id: _, amount: amount_withdrawn}, ticket_table) =
    Ticket_table.remove(split_at, ticket_table) |> Result.get_ok;

  let (withdrawal_handles, withdrawal_handle) =
    Withdrawal_handle_tree.add(
      id => {
        let hash = Withdrawal_handle.hash(~id, ~owner, ~amount, ~ticket);
        {id, hash, owner, amount: amount_withdrawn, ticket};
      },
      t.withdrawal_handles,
    );
  let t = {
    ledger:
      t.ledger
      |> Implicit_address_and_ticket_map.add(
           source,
           ticket,
           source_balance - amount,
         ),
    handles,
  };
  Ok((t, withdrawal_handle));
};

let handles_find_proof = (handle, t) =>
  switch (
    Withdrawal_handle_tree.find(
      handle.Withdrawal_handle.id,
      t.withdrawal_handles,
    )
  ) {
  // TODO: enforce this unreachability on the type system
  // the only way to have a Withdrawal_handle.t is to do a withdraw
  | None => assert(false)
  | Some((proof, _)) => proof
  };
let handles_find_proof_by_id = (key, t) =>
  Withdrawal_handle_tree.find(key, t.withdrawal_handles);
let handles_root_hash = t =>
  Withdrawal_handle_tree.hash(t.withdrawal_handles);
