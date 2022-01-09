open Helpers;
open Crypto;

module Address_and_ticket_map = {
  [@deriving (ord, yojson)]
  type key = {
    address: Address.t,
    ticket: Ticket.t,
  };
  module Map =
    Map.Make_with_yojson({
      [@deriving (ord, yojson)]
      type t = key;
    });
  [@deriving yojson]
  type t = Map.t(Ticket_table.handle);
  let empty = Map.empty;
  let find_opt = (address, ticket) => Map.find_opt({address, ticket});
  let add = (address, ticket) => Map.add({address, ticket});
};
module Handle = {
  [@deriving yojson]
  type t = {
    hash: BLAKE2B.t,
    id: int,
    owner: Tezos.Address.t,
    amount: Amount.t,
    ticket: Ticket.t,
  };
  let hash = (~id, ~owner, ~amount, ~ticket) => {
    let Ticket.{ticketer, data} = ticket;
    Tezos.Deku.Consensus.hash_withdraw_handle(
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
  ledger: Address_and_ticket_map.t,
  ticket_table: Ticket_table.t,
  handles: Handle_tree.t,
};

let empty = {
  ledger: Address_and_ticket_map.empty,
  ticket_table: Ticket_table.empty,
  handles: Handle_tree.empty,
};

let balance = (address, ticket, t) => {
  let balance = {
    let.some handle =
      Address_and_ticket_map.find_opt(address, ticket, t.ledger);
    let.some ticket = Ticket_table.find_opt(handle, t.ticket_table);
    Some(ticket.amount);
  };
  balance |> Option.value(~default=Amount.zero);
};

let get_or_create = (address, ticket, ticket_table, ledger) => {
  switch (Address_and_ticket_map.find_opt(address, ticket, ledger)) {
  | None =>
    let (handle, ticket_table) =
      Ticket_table.add_empty(ticket, ticket_table);
    let ledger = Address_and_ticket_map.add(address, ticket, handle, ledger);
    (handle, ticket_table, ledger);
  | Some(handle) => (handle, ticket_table, ledger)
  };
};

let transfer = (~source, ~destination, amount, ticket, t) => {
  let.ok source_handle =
    Address_and_ticket_map.find_opt(source, ticket, t.ledger)
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
      ledger
      |> Address_and_ticket_map.add(source, ticket, source_handle)
      |> Address_and_ticket_map.add(destination, ticket, destination_handle),
    ticket_table,
    handles: t.handles,
  });
};

// tezos operations
let deposit = (destination, amount, ticket, t) => {
  let (to_merge_handle, ticket_table) =
    Ticket_table.add(
      Ticket_table.unsafe_create_ticket(ticket, amount),
      t.ticket_table,
    );
  let (destination_handle, ticket_table, ledger) =
    get_or_create(destination, ticket, ticket_table, t.ledger);
  let (destination_handle, ticket_table) =
    Ticket_table.join(destination_handle, to_merge_handle, ticket_table)
    |> Result.get_ok;
  {
    ledger:
      ledger
      |> Address_and_ticket_map.add(destination, ticket, destination_handle),
    ticket_table,
    handles: t.handles,
  };
};
let withdraw = (~source, ~destination, amount, ticket, t) => {
  let owner = destination;

  let.ok source_handle =
    Address_and_ticket_map.find_opt(source, ticket, t.ledger)
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

  let (handles, handle) =
    Handle_tree.add(
      id => {
        let hash = Handle.hash(~id, ~owner, ~amount, ~ticket);
        {id, hash, owner, amount: amount_withdrawn, ticket};
      },
      t.handles,
    );
  let t = {
    ledger: t.ledger |> Address_and_ticket_map.add(source, ticket, remaining),
    ticket_table,
    handles,
  };
  Ok((t, handle));
};

let handles_find_proof = (handle, t) =>
  switch (Handle_tree.find(handle.Handle.id, t.handles)) {
  // TODO: enforce this unreachability on the type system
  // the only way to have a Handle.t is to do a withdraw
  | None => assert(false)
  | Some((proof, _)) => proof
  };
let handles_find_proof_by_id = (key, t) => Handle_tree.find(key, t.handles);
let handles_root_hash = t => Handle_tree.hash(t.handles);
