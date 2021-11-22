---
id: optimisation
title: Optimisation guide
---

import Syntax from '@theme/Syntax';


Imagine you develop a contract and try to adhere to all the business requirements while doing it in a secure fashion. If you do not optimise your contract, chances are, it will either fail to fit into the hard limits or be too expensive to execute.

It would be hard to add ad-hoc optimisations after the contract is ready. To avoid such situations, you need to understand what the limits are, how gas is consumed, and how storage and execution fees are computed.

In this article, we will cover Tezos limits, fee model, and the basics of measuring and optimising your contracts.

## Tezos gas model

### What limits and fees are there?

Bounding the contracts' complexity is crucial for public blockchains like Tezos. These bounds ensure liveness – the chain's ability to progress and produce new blocks in a reasonable time.

Limits:
* A limit on the **operation size** is imposed because the nodes transfer operations across the network. If an operation is too large, nodes may fail to broadcast it in time.
* Nodes also need a **block gas limit** to place an upper bound on the number of computations required to verify a block.
* **Operation gas limit** ensures that there is some space in a block to fit more than one operation in.

Fees:
* **Burn fee** is used to prevent blockchain bloat. Users need to _burn_ a certain amount of Tez for each byte of storage they use so that the size of the entire chain stays reasonable.
* **Execution fee** acts as an incentive for the bakers.

### What should I optimise for?

Let us see what these limits and fees depend on:

| Operation type | Limit/fee            | Depends on                  |
|----------------|----------------------|-----------------------------|
| Origination    | Operation size limit | Contract size, storage size |
| Origination    | Burn fee             | Contract size, storage size |
| Origination    | Execution fee        | Contract size, storage size, operations pressure |
| Transaction    | Operation size limit | Parameter size |
| Transaction    | Burn fee             | Bytes added to storage |
| Transaction    | Gas limit            | Consumed gas |
| Transaction    | Execution fee        | Consumed gas, bytes added to storage, operations pressure |

When you **originate** a contract, its code and initial storage are written to the blockchain. Let us define the number of bytes written to storage as `bytes_written`.
* Origination is a single operation, so you must respect the *operation size limit*. The operation comprises the contract's code and initial storage, and some protocol-specific data of constant size, so operation size is `bytes_written + const`.
* During origination, you put some data into the distributed storage, so you will have to *burn* `0.06425ꜩ + 0.00025ꜩ * bytes_written`.
* To make sure your operation gets included into a block, you need to provide an incentive for the bakers. The default baker fee is proportional to the number of bytes written and the amount of gas consumed: `0.0001ꜩ + 100nꜩ/gu * gas + 1000nꜩ/B * bytes_written`. Gas consumption is zero during originations, so the default formula is reduced to `0.0001 ꜩ + 100nꜩ/gu * gas + 1000nꜩ/B * bytes_written`.

Thus, to reduce the origination cost, you need to reduce the size of the contract and the size of the initial storage.

When you make a **transaction** to a contract, things get a little more complicated:
* You still must respect the *operation size limit*, which is proportional to the size of the parameter you pass to the contract. You may hit this limit if you try to pass a huge lambda or a container with a lot of items to a contract.
* If, as a result of executing the contract, the size of the contract's storage exceeds its maximum historical size, you will have to pay a _burn fee_ for the excess bytes (`0.00025ꜩ/B * excess_bytes`). This may be a bit hard to grasp, but if you think about it, this behaviour is reasonable: Tezos burns money for storage no more than once, and never mints Tez back for the storage you free. Imagine the following sequence of operations:
   - The contract gets originated; let us assume the storage size is 500 bytes. The originator burns `0.06425ꜩ + 0.00025ꜩ/B * 500B = 0.18925ꜩ`.
   - During some transaction, the storage size decreases to 400 bytes.
   - You submit an operation that increases the storage size to 505 bytes. Since 500 bytes have been paid for already, you only need to burn `5B * 0.00025ꜩ/B = 0.00125ꜩ`.
* A new kind of limit you must not exceed in transactions is _gas limit._ The consumed gas depends on the amount of computation required to verify your transaction.
* Execution fees depend on the amount of gas consumed (`gas`), and the excess bytes written to the storage (`bytes_written`): `0.0001ꜩ + 100nꜩ/gu * gas + 1000nꜩ/B * bytes_written`. Note that this is just a default formula: bakers are free to include under-priced transactions or choose the transactions with higher fees. Thus, it would make sense to add some buffer if you want to increase the probability that your transaction will be included.

## Optimisation targets

In the previous section, we have identified the following optimisation targets:
1. Gas consumption
2. Contract code size
3. Total storage size
4. Amount of extra bytes written to storage

Another two factors – parameter size and operations pressure – are mostly out of control of the contract author.

Although the optimisation targets listed above are inter-related, it is reasonable to look at them in isolation because the optimisation methods may differ.

### Gas consumption

Contrary to a more conventional instruction-based gas accounting, where each instruction has a cost associated with it, Tezos gas reflects actual computations and I/O operations performed by the nodes. On the one hand, it prevents vulnerabilities caused by incorrect estimation of the instruction costs. On the other hand, it makes the gas model more complex than, for example, the Ethereum one.

To understand how gas is spent, let us look at the phases of transaction execution:
1. Reading the serialised contract code and storage from the context
2. Deserialising the bytes into an intermediate representation
3. Converting untyped intermediate representation of code and storage into typed values
4. Interpreting the instructions
5. Serialising the result of the computation
6. Writing the updated storage bytes back to the context

> Note: when we refer to "storage" and "storage size", we do not include lazy containers (big maps) into these definitions. Although conceptually big maps are a part of storage, they are handled specially.

At each phase, a certain amount of gas is consumed. It would be a rough but useful approximation to say that:
* the amount of gas consumed at phases 1–3 is proportional to `size(code) + size(storage)`
* the interpretation cost (phase 4) is roughly `cost(expensive_instructions) + kε`, where:
  - `cost(expensive_instructions)` is the total cost of some specific expensive instructions – `Tezos.get_contract_opt`, `Bytes.pack`, etc. – invoked by the transaction
  - `k` – the total number of atomic Michelson instructions executed in the transaction.
  - `ε` – the average instruction cost.
* the amount of gas consumed at phases 5–6 is proportional to `size(storage)`.

These approximations are not exactly true: real deserialisation gas consumption also depends on the inherent complexity of the code and data types, converting to a typed representation depends on the actual data and code being converted, and interpreter gas consumption is the sum of the instruction costs. However, such simplified formula makes it possible to lower the dimensionality of the problem and simplify analysis, while not losing much in terms of general trends. For detailed info on gas consumption, please refer to the [Tezos gas model description](https://gitlab.com/tezos/tezos/-/blob/52a074ab3eb43ad0087804b8521f36cb517f7c28/docs/whitedoc/gas_consumption.rst).

According to our approximations, the formula for the total gas consumption would be:
```
α(size(code) + size(storage)) + cost(expensive_instructions) + kε + βsize(storage)
```

In practice, it turns out that if you do not have costly loops with a large number of iterations (i.e., your `k` does not exceed the total number of instructions in the contract by orders of magnitude), the `kε` term of interpreter gas consumption is negligible compared to other costs.

In other words, **the gas consumption depends mostly on the total size of the contract code and storage** (and possibly a small number of expensive instructions, if any). The amount of code _actually executed_ does not affect gas consumption as much.

Here is a list of instructions you should use wisely:
* `Tezos.get_contract_opt` and `Tezos.get_entrypoint_opt` – converting an `address` to a typed contract costs fixed 10000 gas units plus the cost for reading and deserialising the code of the callee. If the callee is large, such operation may consume _a lot_ more than you might expect.
* `Bytes.pack` and `Bytes.unpack` involve serialising and deserialising values, so their cost depends on the size of the data.
* Big map access – reading and updating the values stored in a big map may be expensive.

### Code size

The size of the contract code is arguably the most important optimisation target. When you originate an oversized contract, you risk hitting an operation size limit and pay more for storing the code of the contract in the context. The size of the contract matters in gas consumption as well: the bigger your contract is, the more _gas_ is consumed for reading, deserialising, and type-checking it.

LIGO offers a convenient way to measure the size of the contract code:

```
ligo info measure-contract <SOURCE> --entry-point <ENTRYPOINT>
```

You should try to minimise the size of the code as possible.

### Storage size

There are two types of contract storage in Tezos: regular contract storage and lazy storage – big maps. Although these types are not distinguished at the language level (you can work with big maps as with any regular entry in your storage record), the difference is quite important.

Tezos nodes read, deserialise, and convert all non-lazy storage to typed representation **upon each contract call,** even if a transaction does not read some storage entries. Moreover, after the contract finishes its execution, the contents of the storage are serialised back into bytes and written to the persistent distributive memory, even if the storage was not modified, or just a part of it was modified by the transaction. These actions are quite resource-intensive and cost a significant amount of gas. Thus, **you should do your best to reduce the amount of non-lazy storage** occupied by your contract.

Lazy storage, expressed as big maps, works quite differently. Only those elements that have been explicitly read (using `Big_map.find_opt`) are fetched from the context, deserialised, and converted to typed representation. If the transaction makes any changes to big map entries, only these changed entries are serialised and written to the context.

Why not just use big maps everywhere? Accessing big map elements one-by-one is more expensive than just reading the whole storage in batch. Moreover, big maps are quite restrictive: they do not support iterating over the elements or counting the number of elements. You need to think of how big your containers are, how often the values there are accessed, and what operations you need your containers to support.

Note that allowing users to extend non-lazy containers is insecure: the contract users may add entries to your containers, increasing the gas that would be consumed by your contract in the _next_ transactions. At some point, this may even make the contract stuck: the transactions would fail to fit into the gas limit. Thus, if users can somehow increase the size of the container, you should either use a big map or place a hard limit on the container size.

Is there a list of cases when you should certainly prefer using big map over regular containers such as `list`, `map` or `set`? We would say you should consider using big maps if any of these hold true:
1. The container is extendable by the users.
2. The container is large or unbounded. The precise definition of "large" depends on what is in the container and how many elements are accessed in transactions.
3. You do not use the contents of the container often. For example, you may have just one entrypoint that requires accessing this container, and this entrypoint is called rarely, most probably you would want to use a big map.

### Excess storage

When you make a transaction that writes something to persistent memory, you need to burn a certain amount of Tez. The amount depends on the difference between the number of bytes written and the maximum historical size of the called contract. Such storage burn applies to contract code, regular storage, and lazy storage – basically, any byte that increases the size of the context needs to be paid for. You should try to avoid patterns like event logs that may extend the storage indefinitely.

## Common optimisation techniques

### Constants optimisation

One of the most rewarding ways to optimise your contract is shrinking the constants. For example, if your contract has long, overly-verbose error descriptions passed to `Tezos.failwith`, you should consider replacing them with short abbreviated strings or even integer error codes.

If you have repeating constants (e.g., you may have several entrypoints that check permissions and a constant "PERMISSION_DENIED" error), you can extract these constants to a top-level binding. In this case, the LIGO compiler will generate the code of the form:

| Michelson instruction             | Description                                |
|-----------------------------------|--------------------------------------------|
| `PUSH string "PERMISSION_DENIED"` | Push the error string to stack             |
| ...                               | ...                                        |
| `DIG n`                           | Get the n-th stack entry and put it on top |

This is cheaper than pushing the same string to stack every time it is needed. This string will be pushed to stack _every time_ the contract is called, regardless of whether the current entrypoint actually uses it. This will not increase gas consumption significantly since, as we discussed, the cost of _interpreting_ the instruction is relatively low. However, you can go further and save large constants in _storage_ or even in a big map.

### Inlining

Consider the following contract:

<Syntax syntax="pascaligo">

```pascaligo
function sum (const x : int; const y : int) is x + y

function main (const parameter : int; const storage : int) is
  ((list [] : list (operation)), sum (parameter, storage))
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let sum (x, y : int * int) = x + y

let main (parameter, storage : int * int) =
  ([] : operation list), sum (parameter, storage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let sum = ((x, y): (int, int)) => x + y;

let main = ((parameter, storage): (int, int)) =>
  (([] : list(operation)), sum(parameter, storage));
```

</Syntax>

There are two major ways to represent functions (like `sum`) in Michelson. The first way is to first push the function `f` to the stack, and then execute it with the argument `(parameter, storage)`:

| Michelson instruction | Stack after the instruction                    |
|-----------------------|------------------------------------------------|
|                       | `(parameter, storage)`                         |
| `LAMBDA`<br/>`  (pair int int) int`<br/>`  { UNPAIR; ADD }` | `{ UNPAIR; ADD }`; `(parameter, storage)` |
| `SWAP`                | `(parameter, storage)`; `{ UNPAIR; ADD }`      |
| `EXEC`                | `parameter + storage`                          |
| `NIL operation`       | `[]`, `parameter + storage`                    |
| `PAIR`                | `([], parameter + storage)`                    |


The second way is to replace the function call (`LAMBDA`, `SWAP`, `EXEC` sequence) with the function body, or _inline_ the function:

| Michelson instruction | Stack after the instruction                    |
|-----------------------|------------------------------------------------|
|                       | `(parameter, storage)`                         |
| `UNPAIR`              | `parameter`; `storage`                         |
| `ADD`                 | `parameter + storage`                          |
| `NIL operation`       | `[]`; `parameter + storage`                    |
| `PAIR`                | `([], parameter + storage)`                    |

You may notice that in this case, inlining reduced the size of the contract.

Other declarations can be inlined as well. In this contract, the compiler may generate the code that does `PUSH int 4` twice (in case there is an `[@inline]` annotation), or `PUSH int 4; DUP` (if there is no instruction to inline this binding):

<Syntax syntax="pascaligo">

```pascaligo
const n = 4

function main (const p : int; const s : int) is
  ((list [] : list (operation)), n * n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let n = 4

let main (p, s : unit * int) = ([] : operation list), n * n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let n = 4;

let main = ((p, s): (unit, int)) => (([] : list(operation)), n * n);
```

</Syntax>

LIGO will automatically inline declarations if two conditions are met:
1. The declaration is only used once
2. The declaration is pure, i.e., it does not depend on the execution context or cause failure.

If any of these conditions is not met, LIGO will **not** inline the declaration. You may use the `[@inline]` attribute to force inlining if the declaration is used more than once. You cannot force inlining if the declaration is not pure.

Unfortunately, there is no general rule on when to inline your declarations: sometimes inlining may increase the size of the contract, but in some cases – decrease it.

Intuitively, inlining functions is useful if:
1. You are inlining a function with a complex argument or return type – lambdas in Michelson require an explicit type annotation, and if you inline a function, you can omit it.
2. The function is not used often.

However, the best approach is to measure the gas consumption and the size of your contract to make a decision on inlining.

### Lazy-loading
This peculiar technique can be used to lower the average gas consumption of your contract by making large entrypoints a bit more expensive to call.

Imagine you have a contract with a number of small frequently-used entrypoints and several large entrypoints that are called rarely. During each transaction to the contract, the bakers would read **the whole code** of your contract, deserialise and type-check it, and only after that, execute the requested entrypoint.

<Syntax syntax="pascaligo">

It turns out we can do better. Tezos has a lazy container – big map. The contents of big map are read, deserialised and type-checked during the call to `Big_map.find_opt`, and not at the beginning of the transaction. We can use this container to store the code of our heavy entrypoints: we need to add a `big_map(bool, entrypoint_lambda)` to the storage record, and then use `Big_map.find_opt` to fetch the code of the entrypoint from storage. (Note: in theory, we could use `big_map(unit, entrypoint_lambda)`, but, unfortunately, `unit` type is not comparable, so we cannot use it as a big map index).

Here is how it looks like:

```pascaligo skip
type parameter is LargeEntrypoint of int | ...

type storage is
  record [large_entrypoint : big_map (bool, int -> int); result : int]

function load_large_ep (const storage : storage) is
block {
  const maybe_large_entrypoint : option (int -> int)
  = Map.find_opt (True, storage.large_entrypoint)
} with
    case maybe_large_entrypoint of [
      Some (ep) -> ep
    | None -> (failwith ("Internal error") : int -> int)
    ]

function main (const parameter : parameter; const storage : storage) is
block {
  const nop = (list [] : list (operation))
} with
    case parameter of [
      LargeEntrypoint (n) ->
        (nop, storage with record [result = (load_large_ep (storage)) (n)])
    | ...
      (* Other entrypoints *)
    | ...
    ]
```

</Syntax>
<Syntax syntax="cameligo">

It turns out we can do better. Tezos has a lazy container – big map. The contents of big map are read, deserialised and type-checked during the call to `Big_map.find_opt`, and not at the beginning of the transaction. We can use this container to store the code of our heavy entrypoints: we need to add a `(bool, entrypoint_lambda) big_map` to the storage record, and then use `Big_map.find_opt` to fetch the code of the entrypoint from storage. (Note: in theory, we could use `(unit, entrypoint_lambda) big_map`, but, unfortunately, `unit` type is not comparable, so we cannot use it as a big map index).

Here is how it looks like:
```cameligo skip
type parameter = LargeEntrypoint of int | ...

type storage = { large_entrypoint : (bool, int -> int) big_map; result : int }

let load_large_ep (storage : storage) =
  let maybe_large_entrypoint : (int -> int) option =
    Big_map.find_opt true (storage.large_entrypoint) in
  match maybe_large_entrypoint with
    Some ep -> ep
  | None -> (failwith "Internal error" : (int -> int))

let main (parameter, storage : parameter * storage) =
  match parameter with
    LargeEntrypoint n ->
      ([] : operation list), {storage with result = (load_large_ep storage) n}
  | ...
    (* Other entrypoints *)
  | ...
```

</Syntax>
<Syntax syntax="reasonligo">

It turns out we can do better. Tezos has a lazy container – big map. The contents of big map are read, deserialised and type-checked during the call to `Big_map.find_opt`, and not at the beginning of the transaction. We can use this container to store the code of our heavy entrypoints: we need to add a `big_map(bool, entrypoint_lambda)` to the storage record, and then use `Big_map.find_opt` to fetch the code of the entrypoint from storage. (Note: in theory, we could use `big_map(unit, entrypoint_lambda)`, but, unfortunately, `unit` type is not comparable, so we cannot use it as a big map index).

Here is how it looks like:
```reasonligo skip
type parameter = LargeEntrypoint(int) | ...;

type storage = {large_entrypoint: big_map(bool, (int => int)), result: int };

let load_large_ep = (storage: storage) => {
  let maybe_large_entrypoint: option(int => int) =
    Map.find_opt(true, storage.large_entrypoint);
  switch(maybe_large_entrypoint){
  | Some (ep) => ep
  | None => (failwith("Internal error") : (int => int))
  }
};

let main = ((parameter, storage): (parameter, storage)) => {
  let nop: list(operation) = [];
  switch(parameter){
  | LargeEntrypoint n =>
      (nop, {...storage, result: (load_large_ep(storage))(n)})
  | ...
    /* Other entrypoints */
  | ...
  }
};
```

</Syntax>

We can now put the code of this large entrypoint to storage upon the contract origination. If we do not provide any means to change the stored lambda, the immutability of the contract will not be affected.

<Syntax syntax="pascaligo">

This pattern is also useful if you have long code blocks that repeat across some subset of entrypoints. For example, if you develop a custom token, you may need different flavors of transfers with a common pre-transfer check. In this case, you can add a lambda `preTransferCheck : (transfer_params -> bool)` to the storage and call it upon transfer.

</Syntax>
<Syntax syntax="cameligo">

This pattern is also useful if you have long code blocks that repeat across some subset of entrypoints. For example, if you develop a custom token, you may need different flavors of transfers with a common pre-transfer check. In this case, you can add a lambda `preTransferCheck : (transfer_params -> bool)` to the storage and call it upon transfer.

</Syntax>
<Syntax syntax="reasonligo">

This pattern is also useful if you have long code blocks that repeat across some subset of entrypoints. For example, if you develop a custom token, you may need different flavors of transfers with a common pre-transfer check. In this case, you can add a lambda `preTransferCheck : (transfer_params => bool)` to the storage and call it upon transfer.

</Syntax>

However, you always need to measure the gas consumption and the occupied storage. It may be the case that the wrapper code that extracts the lambda from storage and calls it is costlier than the piece of code you are trying to optimise.

## Conclusion

We have discussed the Tezos fee and gas model and identified the following optimisation targets: contract and storage size, gas consumption, and excess bytes written to storage. We also discussed inlining, constants optimisation, lazy storage, and lazy entrypoint loading. We hope these techniques can help you develop contracts that require fewer resources to execute. And, we cannot stress this enough: **always measure your contracts.**
