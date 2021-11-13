---
id: tz-vs-eth
title: Migrating from Ethereum
---

import Syntax from '@theme/Syntax';


This article is aimed at those who have some experience with developing smart contracts for Ethereum in Solidity. We will cover the key differences between Solidity and LIGO, compare the execution model of Ethereum and Tezos blockchains, and list the features you should be aware of while developing smart contracts for Tezos.

## Languages and libraries

Tezos is an upgradeable blockchain that focuses on decentralisation. It offers a wide variety of languages, frameworks, and tools you can use to develop your contracts. In this article, we mainly focus on the LIGO language, and the provided examples use the Truffle framework for testing. However, many of the points here cover the inherent differences in the blockchain architectures, so they should be valid for other languages and frameworks in the Tezos ecosystem.

The current Tezos protocol uses the Michelson language under the hood. Michelson is much like EVM in Ethereum, inasmuch as its programs are low-level code executed by an embedded virtual machine. Nevertheless, contrary to EVM byte-code, Michelson is a strongly-typed stack-based language designed to be human-readable.

Having a human-readable representation of compiled contracts makes it harder for compiler bugs to pass unnoticed: everyone can review the Michelson code of the contract and even formally prove its correctness.

LIGO is a family of high-level languages. There are several _flavours_ or _syntaxes_ of LIGO – PascaLIGO, CameLIGO, and ReasonLIGO. The developers may choose whatever syntax looks more familiar to them.

## Terminology

For those who come from the Ethereum world, the terminology used in Tezos may be misleading. Tezos developers chose to _not_ reuse the same terms for similar concepts for a reason: a false sense of similarity would be a bad friend for those migrating to a different blockchain architecture.

We will, however, try to associate the terms known to you with the terms used in Tezos. Note that this is just an _association,_ and not the exact equivalence.

| Ethereum term            | Tezos term           | Notes |
|--------------------------|----------------------|-------|
| World state              | Context              | |
| Account                  | Contract or Account  | In Tezos, both smart contracts and accounts controlled by private keys are referred to as "contracts" |
| Externally-owned account | Implicit account     | |
| Contract                 | Smart contract or Originated contract  | |
| Contract deployment      | Contract origination | |
| Transaction              | Operation            | In Tezos, there is a distinction between transactions that transfer value, contract originations, and other kinds of operations |
| –                        | Transaction          | One possible type of operation (value transfer or contract invocation) |
| Miner                    | Baker                | Tezos uses proof-of-stake, so bakers do not solve proof-of-work puzzles. They do produce new blocks and receive rewards, though |
| Contract state           | Contract storage     | |
| Contract method          | Entrypoint           | |
| View method              | – | Currently, Tezos does not provide view functions. You can inspect the storage of the contract, though |

## Types and why they matter

If you come from the Solidity world, you may be accustomed to simple types like `string` or `uint256`, structures, and enums. In LIGO, the types are more advanced. They tend to reflect how the values of these types should be used rather than how they are stored. That is why, for example, LIGO has separate types for `signature` and public `key` instead of just using a byte string (`bytes`) everywhere. Numeric types follow this philosophy: the numbers have arbitrary precision and are, in practice, bounded only by the transaction gas consumption.

Since types are not some purely "technical" concept and have inherent meaning attached to them, it is often a good idea to start thinking about your contract in terms of functions that transform values of one type to values of, possibly, some other type.

You can define new types and type aliases in your code using the `type` keyword:
<Syntax syntax="pascaligo">

```pascaligo
type nat_alias is nat

type token_amount is TokenAmount of nat
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type nat_alias = nat

type token_amount = TokenAmount of nat
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type nat_alias = nat;

type token_amount = TokenAmount(nat);
```

</Syntax>

As in Solidity, there are record types:
<Syntax syntax="pascaligo">

```pascaligo
type creature is record [heads_count : nat; legs_count : nat; tails_count : nat]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type creature = {heads_count : nat; legs_count : nat; tails_count : nat}
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo
type creature = {heads_count: nat, legs_count: nat, tails_count: nat };
```

</Syntax>

There are also _variant_ types (or "sum types") – a more powerful counterpart of Solidity enums that can hold data:

<Syntax syntax="pascaligo">

```pascaligo
type int_option is Number of int | Null

const x : int_option = Number (5)

const y : int_option = Null
```

Valid values of this type are regular numbers wrapped in `Number` (e.g., `Number (5)`, `Number (10)`, etc.) or `Null`. Notice how `Null` does not hold any value.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type int_option = Number of int | Null

let x : int_option = Number 5

let y : int_option = Null
```

Valid values of this type are regular numbers wrapped in `Number` (e.g., `Number 5`, `Number 10`, etc.) or `Null`. Notice how `Null` does not hold any value.

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo
type int_option = Number(int) | Null;

let x: int_option = Number (5);

let y: int_option = Null;
```

Valid values of this type are regular numbers wrapped in `Number` (e.g., `Number (5)`, `Number (10)`, etc.) or `Null`. Notice how `Null` does not hold any value.

</Syntax>

There is a special built-in parameterised `option` type with `Some` and `None` constructors, so we can rewrite the snippet above as:

<Syntax syntax="pascaligo">

```pascaligo
const x : option (int) = Some (5)

const y : option (int) = None
```

This is how we express _nullability_ in LIGO: instead of using a special ad-hoc value like "zero address", we just say it is an `option(address)`. We can then use `case` to see if there is something inside:

```pascaligo
const x : option (int) = Some (5)

const x_or_zero : int
= case x of [
    Some (value) -> value
  | None -> 0
  ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let x : int option = Some 5

let y : int option = None
```

This is how we express _nullability_ in LIGO: instead of using a special ad-hoc value like "zero address", we just say it is an `address option`. We can then use `match` to see if there is something inside:

```cameligo
let x : int option = Some 5

let x_or_zero : int =
  match x with
    Some value -> value
  | None -> 0
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let x: option(int) = Some (5);

let y: option(int) = None;
```

This is how we express _nullability_ in LIGO: instead of using a special ad-hoc value like "zero address", we just say it is an `option(address)`. We can then use `switch` to see if there is something inside:

```reasonligo
let x: option(int) = Some (5);

let x_or_zero: int =
  switch(x){
  | Some (value) => value
  | None => 0
  };
```

</Syntax>

We can go further and combine variant types with records:

<Syntax syntax="pascaligo">

```pascaligo
type committee is record [members : list (address); quorum : nat]

type leader is record [name : string; address : address]

type authority is Dictatorship of leader | Democracy of committee
```

</Syntax>

<Syntax syntax="cameligo">

```cameligo
type committee = {members : address list; quorum : nat}

type leader = {name : string; address : address}

type authority = Dictatorship of leader | Democracy of committee
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo
type committee = {members: list(address), quorum: nat };

type leader = {name: string, address: address };

type authority = Dictatorship(leader) | Democracy(committee);
```

</Syntax>

## Contracts and entrypoints

In Solidity, you usually define a _contract_ with _methods_ and _fields_:

```
contract Counter {
  int public counter;  // storage field

  function increment() public { ... } // method
  function decrement() public { ... } // method
}
```

When the contract is compiled, the Solidity compiler automatically adds dispatching logic into the resulting EVM byte-code. The contract inspects the data passed to it and chooses a method based on the first four bytes of the method's signature hash: `0xbc1ecb8e` means `increment()` and `0x36e44653` means `decrement()`.

In Tezos, a contract must define a default entrypoint that does the dispatching. It is much like a `main` function in C-like languages. It accepts a typed _parameter_ (some data that comes with a transaction) and the current value of the contract _storage_ (the internal state of the contract). The default entrypoint returns a list of internal operations and the new value of the storage.

For example, we can simulate Ethereum dispatching behaviour:
<Syntax syntax="pascaligo">

```pascaligo
function main (const parameter : bytes; const storage : int) is
block {
  const nop : list (operation) = list []
} with
    if (parameter = 0xbc1ecb8e)
    then (nop, storage + 1)
    else
      if (parameter = 0x36e44653)
      then (nop, storage - 1)
      else (failwith ("Unknown entrypoint") : list (operation) * int)
```

However, we can do better. As we discussed, LIGO has a much richer type system than Solidity does. We can encode the entrypoint directly in the parameter type. For our counter contract, we can say, e.g., that the parameter is _either_ `Increment` or `Decrement`, and implement the dispatching logic using `case`:

```pascaligo
type parameter is Increment | Decrement

type storage is int

function main (const p : parameter; const s : storage) is
block {
  const nop : list (operation) = list []
} with
    case p of [
      Increment -> (nop, s + 1)
    | Decrement -> (nop, s - 1)
    ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (parameter, storage : bytes * int) : operation list * int =
  if parameter = 0xbc1ecb8e
  then ([] : operation list), storage + 1
  else
    if parameter = 0x36e44653
    then ([] : operation list), storage - 1
    else (failwith "Unknown entrypoint" : operation list * int)
```

However, we can do better. As we discussed, LIGO has a much richer type system than Solidity does. We can encode the entrypoint directly in the parameter type. For our counter contract, we can say, e.g., that the parameter is _either_ `Increment` or `Decrement`, and implement the dispatching logic using `match`:

```cameligo
type parameter = Increment | Decrement

type storage = int

let main (p, s : parameter * storage) =
  match p with
    Increment -> ([] : operation list), s + 1
  | Decrement -> ([] : operation list), s - 1
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = ((parameter, storage): (bytes, int)) => {
  if (parameter == 0xbc1ecb8e) {
    ([] : list(operation), storage + 1)
  } else {

    if (parameter == 0x36e44653) {
      ([] : list(operation), storage - 1)
    } else {
      (failwith("Unknown entrypoint") : (list(operation), int))
    }
  }
};
```

However, we can do better. As we discussed, LIGO has a much richer type system than Solidity does. We can encode the entrypoint directly in the parameter type. For our counter contract, we can say, e.g., that the parameter is _either_ `Increment` or `Decrement`, and implement the dispatching logic using `switch`:

```reasonligo
type parameter = Increment | Decrement;

type storage = int;

let main = ((p, s): (parameter, storage)) => {
  switch(p){
  | Increment => ([] : list(operation), s + 1)
  | Decrement => ([] : list(operation), s - 1)
  }
};
```

</Syntax>

We do not need any internal operations, since we neither call other contracts nor transfer money. Here is how we can add arguments to our entrypoints:

<Syntax syntax="pascaligo">

```pascaligo
type parameter is Add of int | Subtract of int

type storage is int

function main (const p : parameter; const s : storage) is
block {
  const nop : list (operation) = list []
} with
    case p of [
      Add (n) -> (nop, s + n)
    | Subtract (n) -> (nop, s - n)
    ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type parameter = Add of int | Subtract of int

type storage = int

let main (p, s : parameter * storage) =
  match p with
    Add n -> ([] : operation list), s + n
  | Subtract n -> ([] : operation list), s - n
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo
type parameter = Add(int) | Subtract(int);

type storage = int;

let main = ((p, s): (parameter, storage)) => {
  switch(p){
  | Add n => ([] : list(operation), s + n)
  | Subtract n => ([] : list(operation), s - n)
  }
};
```

</Syntax>

Tezos has special support for parameters encoded with variant types. If the parameter is a variant type, Tezos will treat each constructor as a separate entrypoint (with the first letter lowercased). It is important when we want to call a contract but do not know the full type of its parameter. For example, we can call our counter contract with the following CLI command:

`tezos-client call contract counter from alice --entrypoint '%subtract' --arg 100`

Truffle (and Taquito library, which Truffle for Tezos uses under the hood), also treats entrypoints specially. We can call our `add` entrypoint as follows:
```
const Counter = artifacts.require('Counter')
let counterInstance = await Counter.deployed()
await counterInstance.add(100)
```

## Visibility modifiers

Solidity has visibility modifiers like `private` and `public` for storage entries and contract methods. LIGO has none of these, and you may be wondering why. To answer this, we will first consider storage modifiers and then discuss methods (entrypoints).

It is a popular misconception in the Ethereum world that by marking a storage field `private` you can make this field visible only from inside the contract. Both in Tezos and Ethereum, the contract storage is public. This is due to how blockchains work: nodes need to read contracts' storage to execute and validate the transactions. Tezos allows anyone to inspect storage of any contract with one CLI command. In Ethereum, it is harder but still feasible.

Making a storage field `public` in Solidity instructs the compiler to generate a `view` method that returns the value of this field. In Tezos, it is possible to inspect the storage directly, and it is not possible to return values from contract calls. Thus, public and private storage fields are equivalent in Tezos.

For contract methods, the dispatching logic defines which functions within the contract are accessible from the outside world. Consider the following snippet:

<Syntax syntax="pascaligo">

```pascaligo
function multiplyBy2 (const storage : int) is storage * 2

function multiplyBy4 (const storage : int) is
  multiplyBy2 (multiplyBy2 (storage))

type parameter is MultiplyBy4 | MultiplyBy16

function main (const param : parameter; const storage : storage) is
block {
  const nop : list (operation) = list []
} with
    case param of [
      MultiplyBy4 -> multiplyBy4 (storage)
    | MultiplyBy16 -> multiplyBy4 (multiplyBy4 (storage))
    ]
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let multiplyBy2 (storage : int) : int = storage * 2

let multiplyBy4 (storage : int) : int = multiplyBy2 (multiplyBy2 storage)

type parameter = MultiplyBy4 | MultiplyBy16

let main (param, storage : parameter * storage) =
  ([] : operation list),
  (match param with
     MultiplyBy4 -> multiplyBy4 storage
   | MultiplyBy16 -> multiplyBy4 (multiplyBy4 storage))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let multiplyBy2 = (storage: int) => storage * 2;

let multiplyBy4 = (storage: int) => multiplyBy2(multiplyBy2(storage));

type parameter = MultiplyBy4 | MultiplyBy16;

let main = ((param, storage): (parameter, storage)) => {
  switch(param){
  | MultiplyBy4 => multiplyBy4(storage)
  | MultiplyBy16 => multiplyBy4(multiplyBy4(storage))
  }
};
```

</Syntax>

Here:
1. `multiplyBy2` is _private_ (in Solidity terms): we cannot call it directly from outside of the contract.
2. `multiplyBy4` is _public:_ we can call it both from inside the contract and using the `%multiplyBy4` entrypoint.
3. `%multiplyBy16` is _external:_ there is no function `multiplyBy16` in the contract so we cannot call it from inside the source code, but there is an entrypoint `%multiplyBy16` encoded in the parameter, so we can use tezos-client or Taquito to call it externally.

There is no analogue of `internal` methods in LIGO because LIGO contracts do not support inheritance.

## Lambdas

In Tezos, you can accept _code_ as a parameter. Such functions that you can pass around are called _lambdas_ in functional languages. Let us say that we want to support arbitrary mathematical operations with the counter value. We can just accept the intended formula as the parameter:

<Syntax syntax="pascaligo">

```pascaligo
type parameter is Compute of int -> int // a function that accepts an int and returns an int

type storage is int

function main (const p : parameter; const s : storage) is
  case p of [
    Compute (func) -> ((list [] : list (operation)), func (s))
  ]
```

We can then call this contract with the parameter of the form `Compute (function (const x : int) is x * x + 2 * x + 1)`. Try this out with:
```
ligo run interpret 'main (Compute (function (const x : int) is x * x + 2 * x + 1), 3)' --init-file examples/contracts/ligo/Lambda.ligo
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type parameter = Compute of int -> int // a function that accepts an int and returns an int

type storage = int

let main (p, s : parameter * storage) =
  match p with
  Compute func -> ([] : operation list), func s
```

We can then call this contract with the parameter of the form `Compute (fun (x : int) -> x * x + 2 * x + 1)`. Try this out with:
```
ligo run interpret 'main (Compute (fun (x : int) -> x * x + 2 * x + 1), 3)' --init-file examples/contracts/mligo/Lambda.mligo
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type parameter = Compute((int => int));

type storage = int;

let main = ((p, s): (parameter, storage)) =>
  switch(p){
  | Compute(func) => (([] : list(operation)), func(s))
  };
```

We can then call this contract with the parameter of the form `Compute ((x : int) => x * x + 2 * x + 1)`. Try this out with:
```
ligo run interpret 'main (Compute ((x : int) => x * x + 2 * x + 1), 3)' --init-file examples/contracts/religo/Lambda.religo
```

</Syntax>

The interpreted output is `( LIST_EMPTY() , 16 )`, which is an empty list of operations and the new storage value – the result of the computation.

But this is not all lambdas are capable of. You can, for example, save them in storage:
<Syntax syntax="pascaligo">

```pascaligo
type storage is record [fn : option (int -> int); value : int]

type parameter is CallFunction | SetFunction of int -> int

function call (const fn : option (int -> int); const value : int) is
  case fn of [
    Some (f) -> f (value)
  | None -> (failwith ("Lambda is not set") : int)
  ]

function main (const p : parameter; const s : storage) is
block {
  const newStorage
  = case p of [
      SetFunction (fn) -> s with record [fn = Some (fn)]
    | CallFunction -> s with record [value = call (s.fn, s.value)]
    ]
} with ((list [] : list (operation)), newStorage)
```

Now we can _upgrade_ a part of the implementation by calling our contract with `SetFunction (function (const x : int) => ...)`.

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type storage = {fn : (int -> int) option; value : int}

type parameter = SetFunction of (int -> int) | CallFunction

let call (fn, value : (int -> int) option * int) =
  match fn with
    Some f -> f value
  | None -> (failwith "Lambda is not set" : int)

let main (p, s : parameter * storage) =
  let newStorage =
    match p with
      SetFunction fn -> {s with fn = Some fn}
    | CallFunction -> {s with value = call (s.fn, s.value)} in
  ([] : operation list), newStorage
```

Now we can _upgrade_ a part of the implementation by calling our contract with `SetFunction (fun (x : int) -> ...)`.

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type storage = {fn: option(int => int), value: int };

type parameter = CallFunction | SetFunction((int => int));

let call = ((fn, value): (option((int => int)), int)) => {
  switch(fn){
  | Some (f) => f(value)
  | None => (failwith("Lambda is not set") : int)
  }
};

let main = ((p, s): (parameter, storage)) => {
  let newStorage =
    switch(p){
    | SetFunction fn => {...s, fn: Some (fn)}
    | CallFunction => {...s, value: call(s.fn, s.value)}
    };
  ([] : list(operation), newStorage)
};
```

Now we can _upgrade_ a part of the implementation by calling our contract with `SetFunction ((x : int) => ...)`.

</Syntax>

## Execution model

In Ethereum, you often find yourself "calling" other contracts and splitting your business logic into multiple independent parts. When you call some other contract, the transaction execution is paused until the callee returns the result. We will refer to such invocations as _direct calls:_

```
contract Treasury {
    uint256 public rewardsLeft;
    IBeneficiary beneficiary;

    function disburseRewards() public {
        require(rewardsLeft != 0);
        beneficiary.handleRewards().call.value(rewardsLeft);
        // the execution is paused until `handleRewards` returns
        rewardsLeft = 0;
    }
}
```

Those of you experienced with Solidity may notice that this contract is not reentrancy-safe: the beneficiary contract may utilise the fact that by the time of the call, `rewardsLeft` storage variable has not been updated. The attacker can _call back_ into the caller contract, invoking `disburseRewards` until it drains the treasury contract:

```
contract Beneficiary {
    function handleRewards() public payable {
        Treasury treasury = Treasury(msg.sender);
        if (msg.sender.balance > treasury.rewardsLeft) {
            treasury.disburseRewards();
        }
    }
}
```

In Tezos, the execution model is quite different. Contracts communicate via message passing. Messages are called _internal operations._ If you want to pass a message to another contract, you need to finish the computation first, and then put an operation into the _operations queue._ Here is how it looks like:

<Syntax syntax="pascaligo">

```pascaligo
type storage is record [rewardsLeft : tez; beneficiaryAddress : address]

function treasury (const p : unit; const s : storage) is
block {
  // We do our computations first
  const newStorage = s with record [rewardsLeft = 0mutez];

  // Then we find our beneficiary's `handleRewards` entrypoint:
  const beneficiaryOpt : option (contract (unit))
  = Tezos.get_entrypoint_opt ("%handleTransfer", s.beneficiaryAddress);
  const beneficiary
  = case beneficiaryOpt of [
      Some (contract) -> contract
    | None -> (failwith ("Beneficiary does not exist") : contract (unit))
    ];

  // Then we prepare the internal operation we want to perform
  const operation = Tezos.transaction (Unit, s.rewardsLeft, beneficiary)

  // ...and return both the operations and the updated storage
} with  (list [operation], newStorage)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type storage = {rewardsLeft : tez; beneficiaryAddress : address}

let treasury (p, s : unit * storage) =
  // We do our computations first
  let newStorage = {s with rewardsLeft = 0mutez} in

  // Then we find our beneficiary's `handleRewards` entrypoint:
  let beneficiaryOpt : unit contract option =
    Tezos.get_entrypoint_opt "%handleTransfer" s.beneficiaryAddress in
  let beneficiary =
    match beneficiaryOpt with
      Some contract -> contract
    | None -> (failwith "Beneficiary does not exist" : unit contract) in

  // Then we prepare the internal operation we want to perform
  let operation = Tezos.transaction () s.rewardsLeft beneficiary in

  // ...and return both the operations and the updated storage
  ([operation], newStorage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
type storage = {rewardsLeft: tez, beneficiaryAddress: address };

let treasury = ((p, s): (unit, storage)) => {
  // We do our computations first
  let newStorage = {...s, rewardsLeft: 0mutez};

  // Then we find our beneficiary's `handleRewards` entrypoint:
  let beneficiaryOpt: option(contract(unit)) =
    Tezos.get_entrypoint_opt("%handleTransfer", s.beneficiaryAddress);
  let beneficiary =
    switch(beneficiaryOpt){
    | Some (contract) => contract
    | None => (failwith("Beneficiary does not exist") : contract(unit))
    };

  // Then we prepare the internal operation we want to perform
  let operation = Tezos.transaction((), s.rewardsLeft, beneficiary);
  
  // ...and return both the operations and the updated storage
  ([operation], newStorage)
};
```

</Syntax>

Note that all the state changes occur _before_ the internal operation gets executed. This way, Tezos protects us from unintended reentrancy attacks. However, with complex interactions chain, reentrancy attacks may still be possible.

It is a common idiom in Ethereum to make read-only calls to other contracts. Tezos does not offer a straightforward way to do it but you might think of using something like a callback mechanism:

<Syntax syntax="pascaligo">

```pascaligo skip
type parameter is DoSomething | DoSomethingCont of int

function doSomething (const p : unit; const s : int) is
block {
  (* The callee should call `%doSomethingCont` with the value we want *)
  const op = Tezos.transaction ...
} with (ops, s)

function doSomethingCont (const p : int; const s : int) is
  ((list [] : list (operation)), p + s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
type parameter = DoSomething | DoSomethingCont of int

let doSomething (p, s : unit * int) =
  (* The callee should call `%doSomethingCont` with the value we want *)
  let op = Tezos.transaction ... in
  ([op], s)

let doSomethingCont (p, s : int * int) = ([] : operation list), p + s
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
type parameter = DoSomething | DoSomethingCont(int);

let doSomething = ((p, s): (unit, int)) => {
  /* The callee should call `%doSomethingCont` with the value we want */
  let op = Tezos.transaction ...;
  ([op], s)
}

let doSomethingCont = ((p, s): (int, int)) => (([] : list(operation)), p + s);
```

</Syntax>

However, here you leave your contract in an _intermediate_ state before making an external call. You would need additional precautions to make such callback-style calls secure. In most cases, you should avoid this pattern.

By making contract interactions harder, Tezos incentives you to simplify your architecture. Think about whether you can use lambdas or merge your contracts to avoid complex inter-contract dependencies. If it is possible to _not_ split your logic into multiple contracts, then avoid the split.

You can find more details on how Tezos contracts interact with each other in our [inter-contract calls](https://ligolang.org/docs/tutorials/inter-contract-calls/inter-contract-calls) article.

## Fees

Fee model in Tezos is more complicated than the Ethereum one. The most important bits you should know about are:
1. In Tezos, you _burn_ a certain amount of Tez for increasing the size of the stored data. For example, if you add a new entry to a map or replace a string with a longer one, you must burn your Tez tokens.
2. When you call a contract, the transaction spends gas for reading, deserialising and type-checking the storage. Also, a certain amount of gas gets spent for serialising and writing the storage back to the context. In practice, it means that **the larger your code and storage are, the more expensive it is to call your contract,** regardless of the number of computations performed. If you have big or unbounded containers in storage, you should most probably use `big_map`.
3. Emitting internal operations is very expensive in terms of gas: there is a fixed cost of 10000 gas for `Tezos.get_{contract, entrypoint}_opt` plus the cost of reading, deserialising, and type-checking the parameter of the callee.

Always test for gas consumption and strive to minimise the size of the data stored on chain and the number of internal operations emitted. You can read more on fees in our [Optimisation guide](https://ligolang.org/docs/tutorials/optimisation/optimisation) or in the [Serokell blog post](https://medium.com/tqtezos/how-to-minimize-transaction-costs-of-tezos-smart-contracts-9962347faf64).

## Conclusion

In this article, we discussed some Solidity patterns and their LIGO counterparts. We also covered the most important aspects of the Tezos execution model and fees. Here is a quick reference table comparing Solidity and LIGO patterns:

| Solidity pattern | LIGO pattern |
|------------------|--------------|
| `public` field   | A field in the storage record, e.g. <Syntax syntax="pascaligo"></Syntax>`type storage is record [ x : int; y : nat ]`<Syntax syntax="cameligo">`type storage = { x : int; y : nat }`</Syntax><Syntax syntax="reasonligo">`type storage = { x : int, y : nat }`</Syntax> |
| `private` field  | N/A: all fields are public |
| `private` method | A regular function, e.g., <Syntax syntax="pascaligo">`function func (const a : int) is ...`</Syntax><Syntax syntax="cameligo">`let func (a : int) = ...`</Syntax><Syntax syntax="reasonligo">`let func = (a : int) => ...`</Syntax> |
| `public` /  `external` method  | A separate entrypoint in the parameter: `type parameter = F of int | ...`. `main` entrypoint should dispatch and forward this call to the corresponding function using a match expression |
| `internal` method | There is no concept of inheritance in Tezos |
| Constructor      | Set the initial storage upon origination |
| Method that returns a value | Inspect the contract storage directly |
| `contract.doX(...)` | Emit an internal operation |
| `uint x = contract.getX()` | Do not do this. Think if you can merge the contracts or reverse the execution flow |
| Proxy upgrade pattern | Put lambdas to storage and provide means to update them |
| `emit Event(...)` | Event logs are not supported at the moment. There is a [proposal](https://gitlab.com/tzip/tzip/-/blob/8ac4f90cdc2e3ffb135f6a6a4b3ee0ece3e39870/proposals/tzip-20/tzip-20.md) to support event logs in the future |
