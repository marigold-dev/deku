open Helpers;
include Exn_noop;

module Signed = Signed;
module Signature = Signature;
module Address = Address;
module Wallet = Wallet;
module Ledger = Ledger;
module Ticket = Ticket;
module Validators = Validators;
module Amount = Amount;
module Block = Block;
module Operation = Operation;

include Protocol_state;
let apply_main_chain = (state, operation) => {
  open Operation.Main_chain;
  module Set = Operation_main_chain_set;
  if (Set.mem(operation, state.included_main_operations)) {
    raise(Noop("duplicated operation"));
  };

  let included_main_operations =
    Set.add(operation, state.included_main_operations);
  let state =
    switch (operation.kind) {
    // validators management
    | Deposit({destination, amount, ticket}) =>
      let ledger = Ledger.deposit(destination, amount, ticket, state.ledger);
      {...state, ledger};
    };
  {...state, included_main_operations};
};

// TODO: what should happen if nodes disagree on who should be the current validator?

let maximum_old_block_height_operation = 60L;
let maximum_stored_block_height = 75L; // we're dumb, lots, of off-by-one

let apply_side_chain = (state: t, operation) => {
  open Operation.Side_chain;
  module Set = Operation_side_chain_set;

  // validate operation
  let block_height = operation.block_height;
  if (block_height > state.block_height) {
    raise(Noop("block in the future"));
  };

  if (Int64.add(block_height, maximum_old_block_height_operation)
      < state.block_height) {
    raise(Noop("really old operation"));
  };

  if (Set.mem(operation, state.included_operations)) {
    raise(Noop("duplicated operation"));
  };

  // apply operation
  let included_operations = Set.add(operation, state.included_operations);

  let {source, amount, ticket, _} = operation;
  let.ok (ledger, result) =
    switch (operation.kind) {
    | Transaction({destination}) =>
      let.ok ledger =
        Ledger.transfer(~source, ~destination, amount, ticket, state.ledger);
      Ok((ledger, `Transaction));
    | Withdraw({owner}) =>
      let.ok (ledger, handle) =
        Ledger.withdraw(
          ~source,
          ~destination=owner,
          amount,
          ticket,
          state.ledger,
        );
      // TODO: publish the handle somewhere
      Ok((ledger, `Withdraw(handle)));
    };

  Ok(({...state, ledger, included_operations}, result));
};
let apply_side_chain = (state, operation) =>
  switch (apply_side_chain(state, operation)) {
  | Ok((state, result)) => (state, result)
  | Error(`Not_enough_funds) => raise(Noop("not enough funds"))
  };
let is_next = (state, block) =>
  Int64.add(state.block_height, 1L) == block.Block.block_height
  && state.last_block_hash == block.previous_hash;

let apply_block = (state, block) => {
  Printf.printf("%Ld\n%!", block.Block.block_height);
  let fold_left_noop_when_exception = (f, state, list) =>
    List.fold_left(
      (state, op) =>
        try(f(state, op)) {
        | Noop(_string) =>
          // TODO: print exception
          Printexc.print_backtrace(stdout);
          state;
        },
      state,
      list,
    );
  let state =
    fold_left_noop_when_exception(
      apply_main_chain,
      state,
      block.Block.main_chain_ops,
    );
  let (state, side_chain_operation_results) =
    fold_left_noop_when_exception(
      ((state, results), operation) => {
        let (state, result) = apply_side_chain(state, operation);
        (state, [(operation, result), ...results]);
      },
      (state, []),
      block.Block.side_chain_ops,
    );

  // TODO: move to function trim state
  let state = {
    ...state,
    included_operations:
      state.included_operations
      |> Operation_side_chain_set.filter(op =>
           Int64.sub(state.block_height, op.block_height)
           <= maximum_stored_block_height
         ),
  };

  (
    {
      ...state,
      block_height: block.block_height,
      validators: state.validators |> Validators.update_current(block.author),
      last_block_hash: block.hash,
      last_state_root_update:
        block.state_root_hash != state.state_root_hash
          ? Unix.time() : state.last_state_root_update,
      last_applied_block_timestamp: Unix.time(),
      state_root_hash: block.state_root_hash,
      validators_hash: block.validators_hash,
    },
    side_chain_operation_results,
  );
};

let make = (~initial_block) => {
  let empty = {
    ledger: Ledger.empty,
    included_operations: Operation_side_chain_set.empty,
    included_main_operations: Operation_main_chain_set.empty,
    validators: Validators.empty,
    validators_hash: Validators.hash(Validators.empty),
    block_height: Int64.sub(initial_block.Block.block_height, 1L),
    /* because this calls apply_block internally
       it requires some care to ensure all fields
       are in the right place, otherwise invariants
       can be broken */
    last_block_hash: initial_block.Block.previous_hash,
    state_root_hash: initial_block.Block.state_root_hash,
    last_state_root_update: 0.0,
    last_applied_block_timestamp: 0.0,
  };
  apply_block(empty, initial_block) |> fst;
};
let apply_block = (state, block) => {
  let.assert () = (`Invalid_block_when_applying, is_next(state, block));
  let (valid_hash, hash) =
    if (block.state_root_hash == state.state_root_hash) {
      (true, None);
    } else {
      // TODO: pipeline this
      let (hash, data) = hash(state);
      (block.state_root_hash == hash, Some((hash, data)));
    };
  let.assert () = (`Invalid_state_root_hash, valid_hash);
  let (state, result) = apply_block(state, block);
  Ok((state, hash, result));
};

let get_current_block_producer = state =>
  if (state.last_applied_block_timestamp == 0.0) {
    None;
  } else {
    let diff = Unix.time() -. state.last_applied_block_timestamp;
    // TODO: I'm really into magic numbers
    let skips = Float.to_int(diff /. 10.0);
    Validators.after_current(skips, state.validators);
  };
