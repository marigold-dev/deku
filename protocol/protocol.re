open Helpers;
include Exn_noop;

module Signed = Signed;
module Multisig = Multisig;
module Address = Address;
module Wallet = Wallet;
module Ledger = Ledger;
module Validators = Validators;
module Amount = Amount;
module Block = Block;
module Operation = Operation;

include State;
let apply_main_chain = (state, op) => {
  Operation.Main_chain.(
    switch (op) {
    // funds management
    | Deposit({destination, amount}) =>
      let ledger = Ledger.deposit(~destination, ~amount, state.ledger);
      {...state, ledger};
    | Withdraw({source, amount}) =>
      let ledger = Ledger.withdraw(~source, ~amount, state.ledger);
      {...state, ledger};
    // validators management
    | Add_validator({address, uri}) =>
      let validators = Validators.add({address, uri}, state.validators);
      {...state, validators};
    | Remove_validator({address, uri}) =>
      let validators = Validators.remove({address, uri}, state.validators);
      {...state, validators};
    }
  );
};

// TODO: what should happen if nodes disagree on who should be the current validator?

let maximum_old_block_height_operation = 60L;
let maximum_stored_block_height = 75L; // we're dumb, lots, of off-by-one

let apply_side_chain = (state: t, signed_operation) => {
  open Operation.Side_chain;
  module Set = Operation_side_chain_set;

  // validate operation
  let operation = signed_operation.Self_signed.data;
  let block_height = operation.block_height;
  if (block_height > state.block_height) {
    raise(Noop("block in the future"));
  };

  if (Int64.add(block_height, maximum_old_block_height_operation)
      < state.block_height) {
    raise(Noop("really old operation"));
  };

  if (signed_operation.key != Wallet.get_address(operation.source)) {
    raise(Noop("invalid key signed the operation"));
  };

  if (Set.mem(operation, state.included_operations)) {
    raise(Noop("duplicated operation"));
  };

  // apply operation
  let included_operations = Set.add(operation, state.included_operations);

  let {source, amount, _} = operation;
  let ledger =
    switch (operation.kind) {
    | Transaction({destination}) =>
      Ledger.transfer(~source, ~destination, ~amount, state.ledger)
    | Freeze => Ledger.freeze(~wallet=source, ~amount, state.ledger)
    | Unfreeze => Ledger.unfreeze(~wallet=source, ~amount, state.ledger)
    };

  {...state, ledger, included_operations};
};

let is_next = (state, block) =>
  Int64.add(state.block_height, 1L) == block.Block.block_height
  && state.last_block_hash == block.previous_hash;

let apply_block = (state, block) => {
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
  let state =
    fold_left_noop_when_exception(
      apply_side_chain,
      state,
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

  {
    ...state,
    block_height: block.block_height,
    validators: state.validators |> Validators.update_current(block.author),
    last_block_hash: block.hash,
    last_state_root_update:
      block.state_root_hash != state.state_root_hash
        ? Unix.time() : state.last_state_root_update,
    state_root_hash: block.state_root_hash,
  };
};

let update_pending_state_root_hash = state => {
  let hash = hash(state);
  ({...state, pending_state_root_hash: hash.hash}, hash);
};
let make = (~initial_block) => {
  let empty = {
    ledger: Ledger.empty,
    included_operations: Operation_side_chain_set.empty,
    validators: Validators.empty,
    block_height: Int64.sub(initial_block.Block.block_height, 1L),
    /* because this calls apply_block internally
       it requires some care to ensure all fields
       are in the right place, otherwise invariants
       can be broken */
    last_block_hash: initial_block.Block.previous_hash,
    last_state_root_update: 0.0,
    state_root_hash: SHA256.hash("").hash,
    /* TODO: this a problem because if the state_root_hash is invalid
       that would the hash would still be invalid, so to preserve
       the invariant we need to hash empty */
    pending_state_root_hash: initial_block.Block.state_root_hash,
  };
  let (state, hash) = update_pending_state_root_hash(empty);
  (apply_block(state, initial_block), hash);
};
let apply_block = (state, block) => {
  let.assert () = (`Invalid_block_when_applying, is_next(state, block));
  let.assert () = (
    `Invalid_state_root_hash,
    block.state_root_hash == state.state_root_hash
    || block.state_root_hash == state.pending_state_root_hash,
  );
  // TODO: maybe check if the state root hash could be updated?
  let (state, hash) =
    if (state.pending_state_root_hash == block.state_root_hash) {
      let (state, hash) = update_pending_state_root_hash(state);
      (state, Some(hash));
    } else {
      (state, None);
    };
  let state = apply_block(state, block);
  Ok((state, hash));
};
let next = t => {...t, validators: Validators.next(t.validators)};
