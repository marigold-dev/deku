open Helpers;
include Exn_noop;

module Signature = Protocol_signature;
module Address = Address;
module Wallet = Wallet;
module Ledger = Ledger;
module Ticket_id = Ticket_id;
module Ticket_table = Ticket_table;
module Validators = Validators;
module Amount = Amount;
module Block = Block;
module Operation = Operation;
module Contract = Contract;
module Contract_storage = Contract_storage;
module Interpreter = Interpreter;

include State;
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

let apply_side_chain = {
  let rec fold_left_m = (f, acc, l) =>
    switch (l) {
    | [] => Ok(acc)
    | [x, ...xs] =>
      let.ok acc = f(acc, x);
      fold_left_m(f, acc, xs);
    };
  let rec apply_internal_operation = (state, sender: Address.t, kind) => {
    open Operation.Side_chain;
    let stack_item_to_kind: Interpreter.Types.Stack_item.t => option(kind) =
      fun
      | NonliteralValue(
          Chain_operation(
            Transaction(
              parameter,
              Interpreter.Types.Zinc.Contract.{address, entrypoint},
            ),
          ),
        ) =>
        Some(Transaction({destination: address, parameter, entrypoint}))
      | _ => None;

    let rec map_m = (f, l) =>
      switch (l) {
      | [] => Ok([])
      | [x, ...xs] =>
        let.ok x = f(x);
        let.ok xs = map_m(f, xs);
        Ok([x, ...xs]);
      };

    Printf.printf("%s\n", kind |> kind_to_yojson |> Yojson.Safe.to_string);
    switch (sender, kind) {
    | (_, Withdraw(_))
    | (_, Add_validator(_))
    | (_, Remove_validator(_))
    | (_, Originate_contract(_)) => Error(assert(false))
    | (
        sender,
        Transaction({
          parameter,
          destination: Address.Originated(destination),
          entrypoint: Some(entrypoint),
        }),
      ) =>
      Printf.printf("Transaction!\n");
      let.ok (new_contract_storage, operation_kinds) =
        Contract_storage.update_entry(
          state.contracts_storage,
          destination,
          contract_state => {
            let.ok contract_state =
              Option.to_result(~none=`Invalid_invocation, contract_state);
            module Executor = {
              let get_contract_opt = _ =>
                failwith("Not implemented: get_contract_opt");
              let chain_id = Block.genesis.hash;
              let key_hash = Crypto.Key_hash.of_key;
            };
            open Interpreter;
            open Types;
            let.ok code =
              List.assoc_opt(entrypoint, contract_state.code)
              |> Option.to_result(~none=`Unknown_entrypoint);
            let initial_stack = [
              Stack_item.Record([|parameter, contract_state.storage|]),
            ];
            let initial_state =
              Interpreter.initial_state(~initial_stack, code);
            let interpretation_result =
              Interpreter.eval((module Executor), initial_state);
            Stack_item.(
              switch (interpretation_result) {
              | Success(_, [Record([|List(operations), storage|]), ..._]) =>
                let stack_item_to_kind = x =>
                  stack_item_to_kind(x)
                  |> Option.to_result(~none=`Invalid_invocation);
                let.ok operation_kinds =
                  map_m(stack_item_to_kind, operations);
                Printf.printf(
                  "New storage: %s \n",
                  storage |> Stack_item.to_string,
                );
                Ok(({...contract_state, storage}, operation_kinds));
              | _ => Error(`Invalid_invocation)
              }
            );
          },
        );

      let new_state = {...state, contracts_storage: new_contract_storage};
      let.ok new_state =
        apply_all_internal_operations(
          new_state,
          Address.Originated(destination),
          operation_kinds,
        );
      Ok(new_state);
    | (
        Address.Implicit(sender),
        Transaction({
          parameter: NonliteralValue(Ticket(handle)),
          destination: Implicit(destination),
          entrypoint: None,
        }),
      ) =>
      let.ok ledger =
        Ledger.transfer_ticket(
          ~source=sender,
          ~destination,
          handle,
          state.ledger,
        );
      Ok({...state, ledger});
    | (_, Transaction(_)) => Error(`No_ticket_in_transaction)
    };
  }
  and apply_all_internal_operations = (state, sender, operation_kinds) => {
    fold_left_m(
      (state, operation_kind) =>
        apply_internal_operation(state, sender, operation_kind),
      state,
      operation_kinds,
    );
  };

  (state: t, operation) => {
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
    let state = {...state, included_operations};

    let {source, _} = operation;
    let apply_internal_operation = (state, operation_kind) =>
      apply_internal_operation(
        state,
        Address.Implicit(source),
        operation_kind,
      );

    let update_validators = validators => {
      let last_seen_membership_change_timestamp = Unix.time();
      {...state, validators, last_seen_membership_change_timestamp};
    };

    switch (operation.kind) {
    | Add_validator(validator) =>
      let validators = Validators.add(validator, state.validators);
      Ok((update_validators(validators), `Add_validator));
    | Remove_validator(validator) =>
      let validators = Validators.remove(validator, state.validators);
      Ok((update_validators(validators), `Remove_validator));
    | Originate_contract((code, initial_storage)) =>
      let contract_hash: Crypto.Contract_hash.t =
        operation.hash |> Crypto.BLAKE2B.to_string |> Crypto.BLAKE2B_20.hash;
      let new_address: Address.Originated.t =
        Address.Originated.(of_contract_hash(contract_hash));
      let contract_state =
        Contract_storage.make_state(
          ~entrypoint=None,
          ~code,
          ~storage=initial_storage,
          ~originator=operation.source,
          (),
        );
      let new_contract_state =
        Contract_storage.add(
          state.contracts_storage,
          new_address,
          contract_state,
        );
      Printf.printf(
        "originated contract with address %s\n",
        new_address |> Address.Originated.to_string,
      );
      Ok(({...state, contracts_storage: new_contract_state}, `Origination));
    | Withdraw({owner, amount, ticket}) =>
      let.ok (ledger, handle) =
        Ledger.withdraw(
          ~source,
          ~destination=owner,
          amount,
          ticket,
          state.ledger,
        );
      // TODO: publish the handle somewhere
      Ok(({...state, ledger}, `Withdraw(handle)));
    | Transaction(_) as transaction =>
      let.ok new_state = apply_internal_operation(state, transaction);
      Ok((new_state, `Transaction));
    };
  };
};

let apply_side_chain = (state, operation) =>
  switch (apply_side_chain(state, operation)) {
  | Ok((state, result)) => (state, result)
  | Error(`Invalid_invocation) => raise(Noop("Invalid contract output"))
  | Error(`Not_enough_funds) => raise(Noop("not enough funds"))
  | Error(`Unknown_entrypoint) => raise(Noop("unknown entrypoint"))
  | Error(`No_ticket_in_transaction) =>
    raise(Noop("no ticket in transaction"))
  | Error(`Invalid_ticket) => raise(Noop("no ticket in transaction"))
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
    last_seen_membership_change_timestamp: 0.0,
    contracts_storage: Contract_storage.empty,
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
