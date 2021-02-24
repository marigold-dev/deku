open Exn_noop;

module Z = {
  include Z;
  let to_yojson = z => `String(Z.to_string(z));
  let of_yojson =
    fun
    | `String(string) =>
      try(Ok(Z.of_string(string))) {
      | _ => Error("failed to parse")
      }
    | _ => Error("invalid type");
};

module Block = {
  open Operation;
  type t = {
    block_height: int,
    main_chain_ops: list(Main_chain.t),
    side_chain_ops: list(Signed.t(Side_chain.t)),
  };
};

module Operation_side_chain_set = {
  include Set.Make(Operation.Side_chain);
  let to_yojson = t =>
    t |> to_seq |> List.of_seq |> [%to_yojson: list(Operation.Side_chain.t)];
  let of_yojson = json =>
    json |> [%of_yojson: list(Operation.Side_chain.t)] |> Result.map(of_list);
};
[@deriving yojson]
type t = {
  ledger: Ledger.t,
  // TODO: more efficient lookup on included_operations
  included_operations: Operation_side_chain_set.t,
  validators: list(Address.t),
  current_block_producer: int,
  block_height: int64,
};

let empty = {
  ledger: Ledger.empty,
  included_operations: Operation_side_chain_set.empty,
  validators: [],
  current_block_producer: 0,
  block_height: 0L,
};

let apply_main_chain = (state, op) => {
  open Operation.Main_chain;
  let ledger =
    switch (op) {
    | Deposit({destination, amount}) =>
      Ledger.deposit(~destination, ~amount, state.ledger)
    | Withdraw({source, amount}) =>
      Ledger.withdraw(~source, ~amount, state.ledger)
    };
  {...state, ledger};
};

let maximum_old_block_height_operation = 60L;
let maximum_stored_block_height = 75L; // we're dumb, lots, of off-by-one

let apply_side_chain = (state: t, signed_operation) => {
  open Operation.Side_chain;
  module Set = Operation_side_chain_set;

  // validate operation
  let operation = signed_operation.Signed.data;
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
  let state = {
    ...state,
    included_operations:
      state.included_operations
      |> Operation_side_chain_set.filter(op =>
           Int64.sub(state.block_height, op.block_height)
           <= maximum_stored_block_height
         ),
  };
  {...state, block_height: Int64.add(state.block_height, 1L)};
};
