exception Noop(string);
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
module Address = {
  open Mirage_crypto_pk;

  type key = Rsa.priv;
  type t = Rsa.pub_; // TODO: is okay to have this public
  let to_yojson = t => `String(Rsa.sexp_of_pub(t) |> Sexplib.Sexp.to_string);
  let of_yojson =
    fun
    | `String(sexp) =>
      try(Ok(Sexplib.Sexp.of_string(sexp) |> Rsa.pub_of_sexp)) {
      | _ => Error("failed to parse")
      }
    | _ => Error("invalid type");
};

module Wallet: {
  [@deriving yojson]
  type t;

  // TODO: is this a good idea?
  let of_address: Address.t => t;
  let get_address: t => Address.t;
  module Map: {
    include Map.S with type key = t;
    let to_yojson: ('a => Yojson.Safe.t, t('a)) => Yojson.Safe.t;
    let of_yojson:
      (Yojson.Safe.t => result('a, string), Yojson.Safe.t) =>
      result(t('a), string);
  };
} = {
  [@deriving yojson]
  type t = Address.t;

  let of_address = t => t;
  let get_address = t => t;

  module Map = {
    include Map.Make({
      type t = Address.t;
      let compare = compare;
    });
    let to_yojson = (f, t) =>
      t |> to_seq |> List.of_seq |> [%to_yojson: list((t, 'a))](f);
    let of_yojson = (f, json) =>
      json
      |> [%of_yojson: list((t, 'a))](f)
      |> Result.map(list => list |> List.to_seq |> of_seq);
  };
};

module Amount: {
  [@deriving yojson]
  type t;
  let zero: t;
  let (+): (t, t) => t;
  let (-): (t, t) => t;
  // TODO: is this okay to be public?
  let of_int: int => t;
  // TODO: is this okay to be public?
  let to_int: t => int;
} = {
  [@deriving yojson]
  type t = int;
  let zero = 0;
  let (+) = (+);
  let (-) = (-);
  let of_int = t => {
    // TODO: test this, should amount be non-zero?
    assert(t >= 0);
    t;
  };
  let to_int = t => t;
};

module Ledger: {
  [@deriving yojson]
  type t;
  let empty: t;
  let get_free: (Wallet.t, t) => Amount.t;
  let get_frozen: (Wallet.t, t) => Amount.t;
  let transfer:
    (~source: Wallet.t, ~destination: Wallet.t, ~amount: Amount.t, t) => t;

  let freeze: (~wallet: Wallet.t, ~amount: Amount.t, t) => t;
  let unfreeze: (~wallet: Wallet.t, ~amount: Amount.t, t) => t;

  // on chain ops
  let deposit: (~destination: Wallet.t, ~amount: Amount.t, t) => t;
  let withdraw: (~source: Wallet.t, ~amount: Amount.t, t) => t;
} = {
  open Wallet;
  [@deriving yojson]
  type t = {
    free: Wallet.Map.t(Amount.t),
    frozen: Wallet.Map.t(Amount.t),
  };

  let empty = {free: Map.empty, frozen: Map.empty};

  let get = (address, map) =>
    Map.find_opt(address, map) |> Option.value(~default=Amount.zero);

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
        |> Map.add(source, source_balance - amount)
        |> Map.add(destination, destination_balance + amount),
      frozen: t.frozen,
    };
  };

  let freeze = (~wallet, ~amount, t) => {
    open Amount;

    let source_balance = get_free(wallet, t);
    assert_available(~source=source_balance, ~amount);

    let destination_balance = get_frozen(wallet, t);
    {
      free: t.free |> Map.add(wallet, source_balance - amount),
      frozen: t.frozen |> Map.add(wallet, destination_balance + amount),
    };
  };
  // TODO: avoid this duplicated code
  let unfreeze = (~wallet, ~amount, t) => {
    open Amount;

    let source_balance = get_frozen(wallet, t);
    assert_available(~source=source_balance, ~amount);

    let destination_balance = get_free(wallet, t);

    {
      free: t.free |> Map.add(wallet, destination_balance + amount),
      frozen: t.frozen |> Map.add(wallet, source_balance - amount),
    };
  };

  // tezos operations
  let deposit = (~destination, ~amount, t) => {
    open Amount;
    let destination_balance = get_frozen(destination, t);
    {
      free: t.free,
      frozen: t.frozen |> Map.add(destination, destination_balance + amount),
    };
  };
  let withdraw = (~source, ~amount, t) => {
    open Amount;
    let source_balance = get_frozen(source, t);
    assert_available(~source=source_balance, ~amount);

    {
      free: t.free,
      frozen: t.frozen |> Map.add(source, source_balance - amount),
    };
  };
};

module Operation = {
  type main_chain_ops =
    | Deposit({
        destination: Wallet.t,
        amount: Amount.t,
      })
    | Withdraw({
        source: Wallet.t,
        amount: Amount.t,
      });
  [@deriving yojson]
  type side_chain_ops =
    | Transaction({
        nonce: int32,
        block_height: int64,
        source: Wallet.t,
        destination: Wallet.t,
        amount: Amount.t,
      })
    | Freeze({
        nonce: int32,
        block_height: int64,
        wallet: Wallet.t,
        amount: Amount.t,
      })
    | Unfreeze({
        nonce: int32,
        block_height: int64,
        wallet: Wallet.t,
        amount: Amount.t,
      });
  module Main_chain_ops_set =
    Set.Make({
      type t = main_chain_ops;
      let compare = compare;
    });
  module Side_chain_ops_set =
    Set.Make({
      type t = side_chain_ops;
      let compare = compare;
    });
  module Set = {
    include Set.Make({
      type t = side_chain_ops;
      let compare = compare;
    });
    let to_yojson = t =>
      t |> to_seq |> List.of_seq |> [%to_yojson: list(side_chain_ops)];
    let of_yojson = json =>
      json |> [%of_yojson: list(side_chain_ops)] |> Result.map(of_list);
  };

  let get_side_chain_block_height =
    fun
    | Transaction({block_height, _})
    | Freeze({block_height, _})
    | Unfreeze({block_height, _}) => block_height;
  let get_side_chain_op_source = op => {
    let wallet =
      switch (op) {
      | Transaction({source, _}) => source
      | Freeze({wallet, _})
      | Unfreeze({wallet, _}) => wallet
      };
    Wallet.get_address(wallet);
  };
};
module Block = {
  open Operation;
  type t = {
    block_height: int,
    main_chain_ops: list(main_chain_ops),
    side_chain_ops: list(side_chain_ops),
  };
};

[@deriving yojson]
type t = {
  ledger: Ledger.t,
  // TODO: more efficient lookup on included_operations
  included_operations: Operation.Set.t,
  validators: list(Address.t),
  current_block_producer: int,
  block_height: int64,
};

let empty = {
  ledger: Ledger.empty,
  included_operations: Operation.Set.empty,
  validators: [],
  current_block_producer: 0,
  block_height: 0L,
};

let apply_main_chain = (state, op) => {
  open Operation;
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

let apply_side_chain = (state, op) => {
  open Operation;
  let block_height = Operation.get_side_chain_block_height(op);
  if (block_height > state.block_height) {
    raise(Noop("block in the future"));
  };

  if (Int64.add(block_height, maximum_old_block_height_operation)
      < state.block_height) {
    raise(Noop("really old operation"));
  };
  if (Operation.Set.mem(op, state.included_operations)) {
    raise(Noop("duplicated op"));
  };
  let included_operations = Operation.Set.add(op, state.included_operations);
  let ledger =
    switch (op) {
    | Transaction({nonce: _, block_height: _, source, destination, amount}) =>
      Ledger.transfer(~source, ~destination, ~amount, state.ledger)
    | Freeze({nonce: _, block_height: _, wallet, amount}) =>
      Ledger.freeze(~wallet, ~amount, state.ledger)
    | Unfreeze({nonce: _, block_height: _, wallet, amount}) =>
      Ledger.unfreeze(~wallet, ~amount, state.ledger)
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
      |> Operation.Set.filter(op => {
           let block_height = Operation.get_side_chain_block_height(op);
           Int64.sub(state.block_height, block_height)
           <= maximum_stored_block_height;
         }),
  };
  {...state, block_height: Int64.add(state.block_height, 1L)};
};
