---
id: first-contract
title: First contract
---

import Syntax from '@theme/Syntax';

So far so good, we have learned enough of the LIGO language, we are
confident enough to write out first smart contract.

We will be implementing a counter contract.

## Dry-running a Contract

Testing a contract can be quite easy if we utilise LIGO's built-in dry
run feature. Dry-run works by simulating the main function execution,
as if it were deployed on a real chain. You need to provide the
following:

- `file` - contract to run
- `entrypoint` - name of the function to execute
- `parameter` - parameter passed to the main function (in a
  theoretical invocation operation)
- `storage` - a mock storage value, as if it were stored on a real chain

Here is a full example:


```shell
ligo run dry-run src/basic.ligo Unit Unit --entry-point main
// Outputs:
// tuple[   list[]
//          Unit
// ]
```


Output of the `dry-run` is the return value of our main function, we
can see the operations emitted (in our case an empty list, and the new
storage value being returned) which in our case is still `Unit`.

## A Counter Contract

Our counter contract will store a single `int` as it's storage, and
will accept an `action` variant in order to re-route our single `main`
function to two entrypoints for `add` (addition) and `sub`
(subtraction).

<Syntax syntax="pascaligo">

```pascaligo
type parameter is
  Increment of int
| Decrement of int

type storage is int

type return is list (operation) * storage

function add (const n : int; const store : storage) : storage is store + n
function sub (const n : int; const store : storage) : storage is store - n

function main (const action : parameter; const store : storage) : return is
  ((nil : list(operation)),
   case action of
     Increment (n) -> add (n, store)
   | Decrement (n) -> sub (n, store)
   end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
type parameter =
  Increment of int
| Decrement of int

type storage = int

type return = (operation) list * storage

let add (n, store : int * storage) : storage = store + n
let sub (n, store : int * storage) : storage = store - n

let main (action, store : parameter * storage) : operation list * storage =
  (([]: operation list),
   (match action with
      Increment n -> add (n, store)
    | Decrement n -> sub (n, store)))
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo
type parameter =
  Increment (int)
| Decrement (int)
;

type storage = int;

type return = (list (operation), storage);

let add = ((n, store) : (int, storage)) : storage => store + n;
let sub = ((n, store) : (int, storage)) : storage => store - n;

let main = ((action, store) : (parameter, storage)) : return =>
  (([]: list (operation)),
    (switch (action) {
     | Increment (n) => add ((n, store))
     | Decrement (n) => sub ((n, store))
    }));
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
type parameter =
| ["Increment", int]
| ["Decrement", int]
;

type storage = int;

type return_ = [list<operation>, storage];

let add = ([n, store]: [int, storage]): storage => store + n;
let sub = ([n, store]: [int, storage]): storage => store - n;

let main = ([action, store]: [parameter, storage]): return_ =>
  [list([]) as list <operation>,
    (match (action, {
     Increment: (n: int) => add ([n, store]),
     Decrement: (n: int) => sub ([n, store])
    }))];
```

</Syntax>

To dry-run the counter contract, we will provide the `main` function
with a variant parameter of value `Increment (5)` and an initial
storage value of `5`.

```shell
ligo run dry-run src/counter.ligo "Increment(5)" 5 --entry-point main
// tuple[   list[]
//          10
// ]
```


Our contract's storage has been successfully incremented to `10`.

## Deploying and interacting with a contract on a live-chain

In order to deploy the counter contract to a real Tezos network, we'd
have to compile it first, this can be done with the help of the
`compile-contract` CLI command:

```shell
ligo compile contract src/counter.ligo --entry-point main
```

Command above will output the following Michelson code:


```michelson
{ parameter (or (int %decrement) (int %increment)) ;
  storage int ;
  code { DUP ;
         CDR ;
         DIP { DUP } ;
         SWAP ;
         CAR ;
         IF_LEFT
           { DUP ;
             DIP { DIP { DUP } ; SWAP } ;
             PAIR ;
             DUP ;
             CDR ;
             DIP { DUP ; CAR } ;
             SUB ;
             DIP { DROP 2 } }
           { DUP ;
             DIP { DIP { DUP } ; SWAP } ;
             PAIR ;
             DUP ;
             CDR ;
             DIP { DUP ; CAR } ;
             ADD ;
             DIP { DROP 2 } } ;
         NIL operation ;
         PAIR ;
         DIP { DROP 2 } } }
```

However in order to originate a Michelson contract on Tezos, we also
need to provide the initial storage value, we can use
`compile-storage` to compile the LIGO representation of the storage to
Michelson.

```shell
ligo compile storage src/counter.ligo 5 --entry-point main
// Outputs: 5
```

In our case the LIGO storage value maps 1:1 to its Michelson
representation, however this will not be the case once the parameter
is of a more complex data type, like a record.

## Invoking a LIGO contract

Same rules apply for parameters, as apply for translating LIGO storage
values to Michelson. We will need to use `compile-parameter` to
compile our `action` variant into Michelson, here's how:

```shell
ligo compile parameter src/counter.ligo 'Increment(5)' --entry-point main
// Outputs: (Right 5)
```


Now we can use `(Right 5)` which is a Michelson value, to invoke our
contract - e.g., via `tezos-client`
