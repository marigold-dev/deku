# How validate main chain operations?

```ocaml
let received_this = Withdraw({ amount: 0, wallet: "some-wallet" });
```

Easiest way, we run a full tezos node.

```ocaml
type operation = {
  age : block ;
  content : Withdrawal | Deposit | Validator_change ;
}

type storage = {
  pending_operations = operation queue ;
  validators : validator list ;
  block_hashes : { frozen : hash ; state : hash } list ;
}

type parameter =
  | Block of {
    flush_operation_number : int ;
    frozen_hash : hash ;
    state_hash : hash ;
  }

let flush_operation (op : operation , storage : storage) : storage =
assert op.age <= current_block_level - CONFIRMATION_BLOCKS ;
match op.content with
| Withdrawl -> (* checks frozen hash, send operation *)
| Deposit -> storage (* do nothing, taken care of in sidechain *)
| Validator_change -> new_storage (* process the validator change in storage *)

(* In `flush_operations`, try to actually flush `flush_operation_number` ops *)

let main = fun (param, storage) -> (
  match param with
  | Block { flush_operation_number } -> (
    (* flush `flush_operation_number` of operations in `pending_operations` and check they are all older than `CONFIRMATION_BLOCKS` block *)
  )
)
```

internal chain: maintain full tezos node. when you have a pending operation on mainchain that is older than 10 main-chain blocks, you process it on sidechain + include it in sidechain block. then, when you commit a block to mainchain, you flush on mainchain all operations that were already processed on sidechain.
