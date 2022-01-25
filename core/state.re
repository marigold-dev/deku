open Helpers;
open Crypto;

[@deriving yojson]
type t = {ledger: Ledger.t};

[@deriving yojson]
type receipt =
  | Receipt_tezos_withdraw(Ledger.Handle.t);

let empty = {ledger: Ledger.empty};

let ledger = t => t.ledger;
let hash = t => to_yojson(t) |> Yojson.Safe.to_string |> BLAKE2B.hash;

let apply_tezos_operation = (t, tezos_operation) => {
  open Tezos_operation;
  let apply_internal_operation = (t, internal_operation) => {
    let {ledger} = t;
    switch (internal_operation) {
    | Tezos_deposit({destination, amount, ticket}) =>
      let ledger =
        switch (destination) {
        | Implicit(key_hash) =>
          let destination = Address.of_key_hash(key_hash);
          Ledger.deposit(destination, amount, ticket, ledger);
        | Originated(_) => failwith("not implemented")
        };
      {ledger: ledger};
    };
  };

  let {hash: _, payload} = tezos_operation;
  let {tezos_operation_hash: _, internal_operations} = payload;
  List.fold_left(apply_internal_operation, t, internal_operations);
};
let apply_user_operation = (t, user_operation) => {
  open User_operation;
  let {hash: _, sender, initial_operation} = user_operation;
  switch (initial_operation) {
  | Transaction({destination, amount, ticket}) =>
    let.ok ledger =
      Ledger.transfer(~sender, ~destination, amount, ticket, t.ledger);
    Ok(({ledger: ledger}, None));
  | Tezos_withdraw({owner, amount, ticket}) =>
    let.ok (ledger, handle) =
      Ledger.withdraw(~sender, ~destination=owner, amount, ticket, t.ledger);
    Ok(({ledger: ledger}, Some(Receipt_tezos_withdraw(handle))));
  };
};
let apply_user_operation = (t, user_operation) =>
  switch (apply_user_operation(t, user_operation)) {
  | Ok((t, receipt)) => (t, receipt)
  | Error(`Not_enough_funds) => (t, None)
  };
