# Build state root hash

A Deku state is defined at `src/core_deku/state.ml`. It has type:

```
type t = 
 { 
     ledger: Ledger.t;
     contract_storage: Contract_storage.t
 }
```

To build a state for Deku, we need to build the type `t`. Then using the function `hash state` to get the state of Deku.

## Build `Ledger.t`

Ledger is defined at `src/core_deku/ledger.ml`. It has type:

```
type t = 
 {
    ledger: Address_and_ticket_map.t;
    withdrawal_handles: Withdrawal_handle_tree.t
 }
```

We then need to build the 2 following types:

### Build `Address_and_ticket_map.t`

```
type t = Amount.t Map.t
```

- Where a `Amount.t` (`src/core_deku/amount.ml) is of type:

    ```
    type t = int
    ```

- And `Map.t` 

Map is of type `key`, and type `key` is defined as:

```
type key =
 {
     address: Key_hash.t;
     ticket: Ticket_id.t;
 }
```

### Build `Withdrawal_handle_tree.t`

It is an `Incremental_patricia` tree, defined as:

```
type t = Withdrawal_handle.t
```

- Where `Withdrawal_handle` has type:

    ```
    type t =
    {
        hash: BLAKE2B.t;
        id: int;
        owner: Amount.t;
        ticket: Ticket_id.t
    }
    ```

### Build `ledger` by using `transfer` and `deposit` functions



### Build `withdrawal_handler_tree` by `withdraw` function



<!------------------------------------------------------------------>

## Build `Contract_storage.t`

Contract storage is defined in `src/core_deku/contract_storage.ml`, it has type:

```
type t = Contract.t Map.t
```

- A `Contract.t` is defined at `src/core_deku/contract_vm.ml`:

```
module Contract = struct
  type t =
   { 
       code: Lambda_vm.Ir.code;
       storage: Lambda_vm.Ir.value
   }
```

- Where `Map` is defined as:

```
module Map = Map.Make_with_yojson (Contract_address)
```

A `Contract_address` is defined at `src/core_deku/contract_address.ml`:

```
type t = BLAKE2B_20.t
```
