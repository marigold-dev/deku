---
id: testing
title: Testing LIGO
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

## Testing LIGO code

The LIGO command-line interpreter provides sub-commands to
directly test your LIGO code. The three main sub-commands we currently
support are:

* `test`

* `interpret`

* `dry-run`

We will show how to use the first two, while an example on how to use
the third one was already explained
[here](first-contract.md#dry-running-a-contract).

### Testing with `test`

The sub-command `test` can be used to test a contract using LIGO.

> ⚠️ Please keep in mind that this sub-command is still BETA, and that
> there are features that are work in progress and are subject to
> change. No real test procedure should rely on this sub-command
> alone.

When running the `test` sub-command, LIGO code has access to an
additional `Test` module. This module provides ways of originating
contracts and executing transactions, as well as additional helper
functions that allow to control different parameters of the Tezos
testing library.

> Note:  the LIGO interpreter uses the [same library that Tezos internally uses for testing](https://gitlab.com/tezos/tezos/-/tree/master/src/proto_alpha/lib_protocol/test/helpers).

The function `Test.originate` allows to deploy a contract in the
testing environment. It takes a contract, which is represented as a
function of type `'parameter * 'storage -> operation list * 'storage`,
an initial storage of type `'storage`, and an initial balance for the
contract being deployed. This function deploys the contract, and
returns the type
`('parameter, 'storage) typed_address`, the compiled program in
Michelson of type `michelson_program`, and the size of the program of
type `int`.

The storage of a deployed contract can be queried using the
`Test.get_storage` function, that given a typed address `('parameter,
'storage) typed_address`, returns the `'storage` value.

As a concrete example, suppose we have the following contract:
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This is testnew.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints
function add (const store : storage; const delta : int) : storage is
  store + delta
function sub (const store : storage; const delta : int) : storage is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This is testnew.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This is testme.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

// Two entrypoints
let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
// This is testnew.jsligo
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list<operation>, storage];

// Two entrypoints
let add = ([store, delta]: [storage, int]): storage => store + delta;
let sub = ([store, delta]: [storage, int]): storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store]: [parameter, storage]) : return_ => {
  return [
    list([]) as list<operation>,    // No operations
    match(action, {
      Increment:(n: int) => add ([store, n]),
      Decrement:(n: int) => sub ([store, n]),
      Reset: ()          => 0})
  ]
};
```

</Syntax>


We can deploy it and query the storage right after, to check that the
storage is in fact the one which we started with:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This continues testnew.ligo

const test =
  block {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
    const storage = Test.get_storage(taddr);
  } with (storage = initial_storage);

```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This continues testnew.mligo

let test =
  let initial_storage = 42 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  assert (Test.get_storage taddr = initial_storage)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This continues testnew.religo

let test =
  let initial_storage = 42;
  let (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
  assert (Test.get_storage(taddr) == initial_storage)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
// This continues testnew.jsligo

let _test = () : bool => {
  let initial_storage = 42 as int;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  return (Test.get_storage(taddr) == initial_storage);
};

let test = _test();
```

</Syntax>

The test sub-command will evaluate all top-level definitions and print any
entries that begin with the prefix `test` as well as the value that these
definitions evaluate to. If any of the definitions are found to have
failed, a message will be issued with the line number where the problem
occurred.

<Syntax syntax="pascaligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/testnew.ligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/testnew.mligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/testnew.religo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/testnew.jsligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value true.
```

</Syntax>

The function `Test.transfer_to_contract` allows to bake a transaction.
It takes a target account of type `'parameter contract`, the parameter
of type `'parameter` and an amount of type `tez`. This function
performs the transaction, and returns a `test_exec_result`, which
tells whether the transaction was successful or not, and in case it
was not, it contains a `test_exec_error` describing the error. There
is an alternative version, called `Test.transfer_to_contract_exn`
which performs the transaction and ignores the result, failing in case
that there was an error.

We can extend the previous example by executing a transaction that
increments the storage after deployment:
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This continues testnew.ligo

const test2 =
  block {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(1), 1mutez);
    const storage = Test.get_storage(taddr);
  } with (storage = initial_storage + 1);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This continues testnew.mligo

let test2 =
  let initial_storage = 42 in
  let (taddr, _, _) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn contr (Increment (1)) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 1)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This continues testnew.religo

let test2 =
  let initial_storage = 42;
  let (taddr, _, _) = Test.originate(main, initial_storage, 0tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (1)), 1mutez);
  assert (Test.get_storage(taddr) == initial_storage + 1)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
// This continues testnew.jsligo

let _test2 = () : bool => {
  let initial_storage = 42 as int;
  let [taddr, _, _] = Test.originate(main, initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let r = Test.transfer_to_contract_exn(contr, (Increment (1)), 1 as mutez);
  return (Test.get_storage(taddr) == initial_storage + 1);
}

let test2 = _test2();
```

</Syntax>

The environment assumes a source for the operations which can be set
using the function `Test.set_source : address -> unit`.

#### Unit testing a function

Consider a map binding addresses to amounts and a function removing all entries in that map having an amount less to a given threshold.

<Syntax syntax="cameligo">

```cameligo group=rmv_bal
(*This is remove-balance.mligo*)
type balances = (address, tez) map

let balances_under (b:balances) (threshold:tez) : balances =
  Map.fold
    (fun ((acc, (k, v)) : balances * (address * tez)) -> if v < threshold then Map.remove k acc else acc)
    b b
```

</Syntax>

<Syntax syntax="pascaligo">

```pascaligo group=rmv_bal
(*This is remove-balance.ligo*)
type balances is map (address, tez)

function balances_under (const b : balances ; const threshold : tez) is
  block {
    const f =
      function (const x : balances * (address * tez)) is
        block {
          const (acc, (k,v)) = x ;
        } with if v < threshold then Map.remove (k, acc) else acc ;
  } with Map.fold (f, b, b)
```

</Syntax>

<Syntax syntax="reasonligo">

```reasonligo group=rmv_bal
// This is remove-balance.religo
type balances = map(address, tez);

let balances_under = ( (b, threshold) : (balances, tez) ) : balances =>
  let f = ( (acc,(k,v)) : (balances, (address, tez)) ) =>  if (v < threshold) { Map.remove (k,acc) } else {acc} ;
  Map.fold (f,b,b)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=rmv_bal
// This is remove-balance.jsligo
type balances = map <address, tez>

let balances_under = (b : balances, threshold:tez) : balances => {
  let f = (acc : balances, kv :[address , tez] ) : balances => {
    let [k,v] = kv ;
    if (v < threshold) { return Map.remove (k,acc) } else {return acc}
  };
  return Map.fold (f,b,b);
}
```

</Syntax>

Let us imagine that we want to test this function against a range of thresholds with the LIGO test framework.

<!-- I divided unit-remove-balance in multiple part of clarity -->
First, let's include the file under test and reset the state with 5 bootstrap accounts (we are going to use
the bootstrap addresses later)

<Syntax syntax="cameligo">

```cameligo test-ligo group=rmv_bal_test
#include "./gitlab-pages/docs/advanced/src/remove-balance.mligo"
let _u = Test.reset_state 5n ([] : tez list)
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=rmv_bal_test
#include "./gitlab-pages/docs/advanced/src/remove-balance.ligo"
const _u = Test.reset_state (5n, (list [] : list (tez)))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=rmv_bal_test
#include "./gitlab-pages/docs/advanced/src/remove-balance.religo"
let _u = Test.reset_state (5n, ([] : list(tez)));
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=rmv_bal_test
#include "./gitlab-pages/docs/advanced/src/remove-balance.jsligo"
let x = Test.reset_state ( 5 as nat, list([]) as list <tez> );
```

</Syntax>

Now build the `balances` map that will serve as the input of our test.

<Syntax syntax="cameligo">

```cameligo test-ligo group=rmv_bal_test
let balances : balances =
  let (a1, a2, a3) = (Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3) in
  Map.literal [ (a1 , 10tz ) ; (a2, 100tz ) ; (a3, 1000tz ) ]
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=rmv_bal_test
const balances : balances = block {
  const a1 = Test.nth_bootstrap_account(1);
  const a2 = Test.nth_bootstrap_account(2);
  const a3 = Test.nth_bootstrap_account(3); } with
  (map [ a1 -> 10tz ; a2 -> 100tz ; a3 -> 1000tz ])
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=rmv_bal_test
let balances : balances =
  let (a1, a2, a3) = (Test.nth_bootstrap_account(1), Test.nth_bootstrap_account(2), Test.nth_bootstrap_account(3));
  Map.literal([ (a1 , 10tz ) , (a2, 100tz ) , (a3, 1000tz ) ]);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=rmv_bal_test
let balances : balances =
  Map.literal(list([[Test.nth_bootstrap_account(1), 10 as tez],
                    [Test.nth_bootstrap_account(2), 100 as tez],
                    [Test.nth_bootstrap_account(3), 1000 as tez]]));
```

</Syntax>

Our simple test loop will call `balances_under` with the compiled map
defined above, get the size of the resulting map and compare it to an
expected value with `Test.michelson_equal`.

The call to `balance_under` and the computation of the size of the resulting map is achieved through the primitive `Test.run`.
This primitive runs a function on an input, translating both (function and input)
to Michelson before running on the Michelson interpreter.  
More concretely `Test.run f v` performs the following:

1. Compiles the function argument `f` to Michelson `f_mich`
2. Compiles the value argument `v` (which was already evaluated) to Michelson `v_mich`
3. Runs the Michelson interpreter on the code `f_mich` with the initial stack `[ v_mich ]`

The function that is being compiled is called `tester`.

We also print the actual and expected sizes for good measure.


<Syntax syntax="cameligo">

```cameligo test-ligo group=rmv_bal_test
let test =
  List.iter
    (fun ((threshold , expected_size) : tez * nat) ->
      let tester (balances, threshold : balances * tez) = Map.size (balances_under balances threshold) in
      let size = Test.run tester (balances, threshold) in
      let expected_size = Test.eval expected_size in
      let () = Test.log ("expected", expected_size) in
      let () = Test.log ("actual",size) in
      assert (Test.michelson_equal size expected_size)
    )
    [(15tez,2n);(130tez,1n);(1200tez,0n)]
```

</Syntax>
<Syntax syntax="pascaligo">

```pascaligo test-ligo group=rmv_bal_test
const test =
  List.iter (
    (function (const threshold : tez ; const expected_size : nat) is
      block {
        function tester(const input : (balances * tez)) is Map.size(balances_under(input.0, input.1));
        const size_ = Test.run(tester, (balances, threshold));
        const expected_size = Test.eval(expected_size);
        Test.log (("expected", expected_size));
        Test.log (("actual", size_));
      } with
        assert (Test.michelson_equal (size_, expected_size))),
    list [(15tez, 2n); (130tez, 1n); (1200tez, 0n)])
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=rmv_bal_test
let test =
  List.iter (
    (((threshold , expected_size) : (tez, nat)) =>
      let tester = ((balances, threshold) : (balances, tez)) => Map.size (balances_under (balances, threshold));
      let size = Test.run(tester, (balances, threshold));
      let expected_size = Test.eval(expected_size) ;
      let _u = Test.log (("expected", expected_size)) ;
      let _u = Test.log (("actual", size)) ;
      assert ( Test.michelson_equal (size, expected_size) )),
    [ (15tez, 2n), (130tez, 1n), (1200tez, 0n)] );
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=rmv_bal_test
let test =
  List.iter
    ( ([threshold , expected_size] : [tez , nat]) : unit => {
      let tester = ([balances, threshold] : [balances, tez]) : nat => Map.size (balances_under (balances, threshold));
      let size = Test.run(tester, [balances, threshold]);
      let expected_size_ = Test.eval(expected_size) ;
      let unit = Test.log (["expected", expected_size]) ;
      let unit_ = Test.log (["actual",size]) ;
      return (assert (Test.michelson_equal (size,expected_size_)))
    },
    list ([ [15 as tez,2 as nat] , [130 as tez,1 as nat] , [1200 as tez,0 as nat]]) );
```

</Syntax>

You can now execute the test:

<Syntax syntax="cameligo">

```shell
> ligo run test gitlab-pages/docs/advanced/src/unit-remove-balance-mixed.mligo
// Outputs:
// ("expected" , 2)
// ("actual" , 2)
// ("expected" , 1)
// ("actual" , 1)
// ("expected" , 0)
// ("actual" , 0)
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>

<Syntax syntax="pascaligo">

```shell
> ligo run test gitlab-pages/docs/advanced/src/unit-remove-balance-mixed.ligo
// Outputs:
// ("expected" , 2)
// ("actual" , 2)
// ("expected" , 1)
// ("actual" , 1)
// ("expected" , 0)
// ("actual" , 0)
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>

<Syntax syntax="reasonligo">

```shell
> ligo run test gitlab-pages/docs/advanced/src/unit-remove-balance-mixed.religo
// Outputs:
// ("expected" , 2)
// ("actual" , 2)
// ("expected" , 1)
// ("actual" , 1)
// ("expected" , 0)
// ("actual" , 0)
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>

<Syntax syntax="jsligo">

```shell
> ligo run test gitlab-pages/docs/advanced/src/unit-remove-balance-mixed.jsligo
// Outputs:
// ("expected" , 2)
// ("actual" , 2)
// ("expected" , 1)
// ("actual" , 1)
// ("expected" , 0)
// ("actual" , 0)
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>

### Testing with `interpret`

The sub-command `interpret` allows to interpret an expression in a
context initialised by a source file. The interpretation is done using
Michelson's interpreter.

We can see how it works on an example. Suppose we want to test the following
contract.

<Syntax syntax="pascaligo">

```pascaligo
// This is testme.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

// Two entrypoints
function add (const store : storage; const delta : int) : storage is
  store + delta
function sub (const store : storage; const delta : int) : storage is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
// This is testme.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
// This is testme.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

// Two entrypoints
let add = ((store, delta) : (storage, int)) : storage => store + delta;
let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))
  | Reset         => 0}))
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
// This is testme.jsligo
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list<operation>, storage];

// Two entrypoints
let add = ([store, delta]: [storage, int]): storage => store + delta;
let sub = ([store, delta]: [storage, int]): storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store]: [parameter, storage]) : return_ => {
  return [
    list([]) as list<operation>,    // No operations
    match(action, {
      Increment:(n: int) => add ([store, n]),
      Decrement:(n: int) => sub ([store, n]),
      Reset: ()          => 0})
  ]
};
```

</Syntax>

This contract keeps an integer as storage, and has three entry-points:
one for incrementing the storage, one for decrementing the storage,
and one for resetting the storage to `0`.

As a simple property, we check whether starting with a storage of
`10`, if we execute the entry-point for incrementing `32`, then we get
a resulting storage of `42`. For checking it, we can interpret the
`main` function:

<Syntax syntax="pascaligo">

```shell
ligo run interpret "main (Increment (32), 10)" --init-file gitlab-pages/docs/advanced/src/testing/testme.ligo
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo run interpret "main (Increment (32), 10)" --init-file testme.mligo
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo run interpret "main (Increment (32), 10)" --init-file testme.religo
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo run interpret "main (Increment (32), 10)" --init-file testme.jsligo
// Outputs:
// ( LIST_EMPTY() , 42 )
```

</Syntax>

With the argument `--init-file` we pass the contract we want to test,
and the sub-command requires also the expression to evaluate in that
context, in this case, a call to our contract (`main`) with parameter
`Increment (32)` and storage `10`. As a result, we can check that the
resulting storage is `42` (the second component of the pair), and
there are no further operations to execute (the first component).

We can tune certain parameters of the execution by passing them as
arguments:

```
--amount=AMOUNT (absent=0)
    AMOUNT is the amount the Michelson interpreter will use for the
    transaction.
--balance=BALANCE (absent=0)
    BALANCE is the balance the Michelson interpreter will use for the
    contract balance.
--now=NOW
    NOW is the NOW value the Michelson interpreter will use
    (e.g. '2000-01-01T10:10:10Z')
--sender=SENDER
    SENDER is the sender the Michelson interpreter transaction will use.
--source=SOURCE
    SOURCE is the source the Michelson interpreter transaction will use.
```
