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
        Some(Invocation({destination: address, parameter, entrypoint}))
      | _ => None;

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
  let update_validators = validators => {
    let last_seen_membership_change_timestamp = Unix.time();
    {...state, validators, last_seen_membership_change_timestamp};
  };
  switch (operation.kind) {
  | Transaction({destination, amount, ticket}) =>
    let.ok ledger =
      Ledger.transfer(~source, ~destination, amount, ticket, state.ledger);
    Ok(({...state, ledger}, `Transaction));
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
  | Add_validator(validator) =>
    let validators = Validators.add(validator, state.validators);
    Ok((update_validators(validators), `Add_validator));
  | Remove_validator(validator) =>
    let validators = Validators.remove(validator, state.validators);
    Ok((update_validators(validators), `Remove_validator));
  | Originate_contract((code, initial_storage)) =>
    let (_, signature) =
      Protocol_signature.signature_to_signature_by_address(
        operation.signature,
      );
    let new_address =
      Crypto.Signature.to_string(signature) |> Crypto.BLAKE2B_20.hash;
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
    Ok(({...state, contracts_storage: new_contract_state}, `Origination));
  | Invoke_contract(contract_hash, parameter) =>
    exception Invalid_contract_output;
    try({
      let new_contract_storage =
        Contract_storage.update(
          state.contracts_storage,
          contract_hash,
          contract_state => {
            let.some contract_state = contract_state;
            module Executor = {
              let get_contract_opt = _ =>
                failwith("Not implemented: get_contract_op");
              let chain_id = Block.genesis.hash;
              let hash = _ => failwith("Not implemented: hash");
              let key_hash = Crypto.Key_hash.of_key;
            };
            open Interpreter;
            let initial_stack = [
              Types.Stack_item.Record([|parameter, contract_state.storage|]),
            ];
            let initial_state =
              Interpreter.initial_state(~initial_stack, contract_state.code);
            let interpretation_result =
              Interpreter.eval((module Executor), initial_state);
            open Types.Interpreter_output;
            open Types.Stack_item;
            let (_operations, new_storage) =
              switch (interpretation_result) {
              | Success(
                  _,
                  [Record([|List(operations), new_storage|]), ..._],
                ) => (
                  operations,
                  new_storage,
                )
              | _ => raise(Invalid_contract_output)
              };

            Some({...contract_state, storage: new_storage});
          },
        );
      Ok(({...state, contracts_storage: new_contract_storage}, `Invocation));
    }) {
    | _ => Error(`Invalid_invocation)
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
  | Error(`Invalid_argument_to_implicit_account) =>
    raise(Noop("Invalid argument passed to implicit account"))
  | Error(`Tried_to_use_entrypoint_for_implicit_account) =>
    raise(
      Noop(
        "unknown entrypoint (on an implicit account, None is the only entrypoint)",
      ),
    )
  | Error(`Invoked_contract_without_entrypoint) =>
    raise(Noop("Invoked contract without entrypoint"))
  | Error(`Implicit_account_invoking_contract_with_ticket) =>
    raise(Noop("Implicit account invoking contract with ticket"))
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
