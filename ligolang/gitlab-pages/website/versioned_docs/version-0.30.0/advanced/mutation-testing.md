---
id: mutation-testing
title: Mutation testing
---

import Syntax from '@theme/Syntax';
import Link from '@docusaurus/Link';

We assume that the reader is familiar with LIGO's testing framework. A
reference can be found [here](testing.md).

## A simple testing example

To demonstrate how to use the mutation primitives in the testing
framework, we will have a look at a basic function that we would like
to test. Suppose we want to construct a function that takes an integer
argument and doubles it, tentatively the following one:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
function twice (const x : int) : int is
  x + x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
let twice (x : int) = x + x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
let twice = (x : int) : int => x + x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
let twice = (x: int): int => x + x;
```

</Syntax>

Assume that we want to make sure that this function works as expected,
because it will be used as part of a major development. We could write
the following tests:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
function simple_tests(const f : int -> int) is
  block {
    (* Test 1 *)
    assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
    (* Test 2 *)
    assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
  } with unit;

const test = simple_tests(twice);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = assert (Test.michelson_equal (Test.run f 0) (Test.eval 0)) in
  (* Test 2 *)
  let () = assert (Test.michelson_equal (Test.run f 2) (Test.eval 4)) in
  ()

let test = simple_tests twice
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
let simple_tests = (f : (int => int)) => {
  /* Test 1 */
  assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
  /* Test 2 */
  assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
};

let test = simple_tests(twice);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage
let simple_tests = (f : ((input: int) => int)) : unit => {
  /* Test 1 */
  assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
  /* Test 2 */
  assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
};

let test = simple_tests(twice);
```

</Syntax>

These tests check that `twice`:
- when run on input `0`, it returns `0`.
- when run on input `2`, it returns `2`.

The function implemented (`twice`) above passes the tests:

<Syntax syntax="pascaligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.ligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.mligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.religo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.jsligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
```

</Syntax>

The implementation is, in fact, correct. However, it is easy to
make a mistake and write the following implementation instead:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
function twice (const x : int) : int is
  x * x
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
let twice (x : int) = x * x
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
let twice = (x : int) : int => x * x;
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage2
let twice = (x: int): int => x * x;
```

</Syntax>

And, in fact, when we run `simple_tests` on this faulty
implementation, we will see that it also passes the tests.

This is because `0 * 0 = 0 + 0 = 0` and `2 * 2 = 2 + 2 = 4`. What
lessons can we draw from this?

The function was tested, but nothing guaranteed that
the tests are complete enough.

Mutation testing tries to help in this area by modifying functions
while keeping the same tests fixed, and alerting if some of the
modified functions pass all of the tests: in that situation, the tests
were not good enough to separate a good implementation from the
(possibly) incorrect ones.

We can see now how to do mutation testing in LIGO for the original
implementation for `twice` (`x + x`). The primitive from the testing
framework that we will use is

<Syntax syntax="pascaligo">

```pascaligo skip
Test.mutation_test : 'a -> ('a -> 'b) -> option ('b * mutation)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
Test.mutation_test : 'a -> ('a -> 'b) -> ('b * mutation) option
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
Test.mutation_test : ('a, ('a -> 'b)) => option ('b, mutation)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
Test.mutation_test : (value: 'a, tester: ('a -> 'b)) => option <['b, mutation]>
```

</Syntax>

which takes a value to mutate and and a function to apply to altered
versions of that value (testing function). As soon as the function
correctly terminates (i.e. does not fail) in some value mutation,
`Test.mutation_test` will stop and return the result of the function
application, together with a `mutation` describing the change in the
value. If all of the mutations tested fail, then `Test.mutation_test`
will return `None`.

Typically, the values to mutate are functions (i.e. `'a` will be
a function type), and these functions' return type (i.e. `'b`) will be
`unit`.

For the example above, the function that will be applied is `simple_tests`,
and the value to mutate is `twice`:

<Syntax syntax="pascaligo">

```pascaligo skip
const test_mutation =
  case Test.mutation_test(twice, simple_tests) of
    None -> unit
  | Some (_, mutation) -> block { Test.log(mutation) }
                          with failwith("Some mutation also passes the tests! ^^")
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let test_mutation =
  match Test.mutation_test twice simple_tests with
    None -> ()
  | Some (_, mutation) -> let () = Test.log(mutation) in
                          failwith "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let test_mutation =
  switch(Test.mutation_test(twice, simple_tests)) {
  | None => ()
  | Some (_, mutation) => { Test.log(mutation);
                            failwith ("Some mutation also passes the tests! ^^") }
  };
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let test_mutation =
  match(Test.mutation_test(twice, simple_tests), {
    None: () => unit,
    Some: pmutation => { Test.log(pmutation[1]);
                         failwith ("Some mutation also passes the tests! ^^") }
  });
```

</Syntax>

Running the tests again, the following output is obtained:

<Syntax syntax="pascaligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.ligo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation.ligo", line 2, characters 2-7:
//   1 | function twice (const x : int) : int is
//   2 |   x + x
//   3 |
//
// Replacing by: MUL(x ,
// x).
// File "gitlab-pages/docs/advanced/src/mutation.ligo", line 20, characters 31-82:
//  19 |   | Some (_, mutation) -> block { Test.log(mutation) }
//  20 |                           with failwith("Some mutation also passes the tests! ^^")
//  21 |   end
//
// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.mligo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation.mligo", line 1, characters 22-27:
//   1 | let twice (x : int) = x + x
//   2 |
//
// Replacing by: MUL(x ,
// x).
// File "gitlab-pages/docs/advanced/src/mutation.mligo", line 17, character 26 to line 18, character 76:
//  16 |     None -> ()
//  17 |   | Some (_, mutation) -> let () = Test.log(mutation) in
//  18 |                           failwith "Some mutation also passes the tests! ^^"
//
// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.religo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation.religo", line 1, characters 25-30:
//   1 | let twice = (x : int) => x + x;
//   2 |
//
// Replacing by: MUL(x ,
// x).
// File "gitlab-pages/docs/advanced/src/mutation.religo", line 18, characters 28-80:
//  17 |   | Some (_, mutation) => { Test.log(mutation);
//  18 |                             failwith ("Some mutation also passes the tests! ^^") }
//  19 |   };
//
// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.jsligo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation.jsligo", line 1, characters 31-36:
//   1 | let twice = (x : int) : int => x + x;
//   2 |
//
// Replacing by: MUL(x ,
// x).
// File "gitlab-pages/docs/advanced/src/mutation.jsligo", line 18, characters 25-77:
//  17 |     Some: pmutation => { Test.log(pmutation[1]);
//  18 |                          failwith ("Some mutation also passes the tests! ^^") }
//  19 |   });
//
// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>

The primitive `Test.mutation_test` tries out various mutations on
`twice`, and sees if they pass all of the tests. In this scenario, it
was discovered that the mutation `MUL(x,x)` also passes the tests:
this is the precise case we discussed earlier, when the incorrect
implementation `x * x` would not be detected by the tests. We need to
update the test suite. In this case, we could propose to add a new
test:

<Syntax syntax="pascaligo">

```pascaligo skip
function simple_tests(const f : int -> int) is
  block {
    (* Test 1 *)
    assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
    (* Test 2 *)
    assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
    (* Test 3 *)
    assert (Test.michelson_equal(Test.run(f, 1), Test.eval(2)));
  } with unit;
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = assert (Test.michelson_equal (Test.run f 0) (Test.eval 0)) in
  (* Test 2 *)
  let () = assert (Test.michelson_equal (Test.run f 2) (Test.eval 4)) in
  (* Test 3 *)
  let () = assert (Test.michelson_equal (Test.run f 1) (Test.eval 2)) in
  ()
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let simple_tests = (f : (int => int)) => {
  /* Test 1 */
  assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
  /* Test 2 */
  assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
  /* Test 3 */
  assert (Test.michelson_equal(Test.run(f, 1), Test.eval(2)));
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let simple_tests = (f : ((input: int) => int)) : unit => {
  /* Test 1 */
  assert (Test.michelson_equal(Test.run(f, 0), Test.eval(0)));
  /* Test 2 */
  assert (Test.michelson_equal(Test.run(f, 2), Test.eval(4)));
  /* Test 3 */
  assert (Test.michelson_equal(Test.run(f, 1), Test.eval(2)));
};
```

</Syntax>

this verifies that when input `1` is given, output `2` is returned.
Running the mutation testing again after this adjustment, no mutation
(among those tried) will pass the tests, giving extra confidence in
the tests proposed:

<Syntax syntax="pascaligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.ligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
// - test_mutation exited with value ().
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.mligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
// - test_mutation exited with value ().
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.religo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
// - test_mutation exited with value ().
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation.jsligo
// Outputs:
// Everything at the top-level was executed.
// - test exited with value ().
// - test_mutation exited with value ().
```

</Syntax>

## Mutating a contract

The following is an example on how to mutate a contract. For that, we
will use a variation of the canonical LIGO contract with only two
entrypoints `Increment` and `Decrement`:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This is testnew.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int

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
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This is mutation-contract.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int

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
 | Decrement (n) -> sub (store, n))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This is mutation-contract.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int);

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
  | Decrement (n) => sub ((store, n))}))
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage3
// This is mutation-contract.jsligo
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int];

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
      Decrement:(n: int) => sub ([store, n])})
  ]
};
```

</Syntax>

Doing mutation testing on a contract with multiple entrypoints can
help in finding out entrypoints that are not covered by the tests.

Consider the following test, which deploys a contract passed as
an argument (of the same type as `main` above), and then tests that
the entrypoint `Increment(7)` works as intended on an initial storage
`5`:

<Syntax syntax="pascaligo">

```pascaligo test-ligo group=frontpage
// This continues mutation-contract.ligo

function originate_and_test(const mainf : parameter * storage -> return) is
  block {
    const initial_storage = 5;
    const (taddr, _, _) = Test.originate(mainf, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(7), 1mutez);
    const storage = Test.get_storage(taddr);
    assert (storage = initial_storage + 7);
  } with (unit);

const test = originate_and_test(main);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo test-ligo group=frontpage
// This continues mutation-contract.mligo

let originate_and_test (mainf : parameter * storage -> return) =
  let initial_storage = 7 in
  let (taddr, _, _) = Test.originate mainf initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn contr (Increment (7)) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 7)

let test = originate_and_test main
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo test-ligo group=frontpage
// This continues mutation-contract.religo

let originate_and_test = (mainf : (parameter, storage) => return) => {
  let initial_storage = 5;
  let (taddr, _, _) = Test.originate(mainf, initial_storage, 0tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (7)), 1mutez);
  assert (Test.get_storage(taddr) == initial_storage + 7)
};

let test = originate_and_test(main);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo test-ligo group=frontpage3
// This continues mutation-contract.jsligo

let originate_and_test = (mainf : ((p: parameter, s: storage) => return_)) : unit => {
  let initial_storage = 5 as int;
  let [taddr, _, _] = Test.originate(mainf, initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let r = Test.transfer_to_contract_exn(contr, (Increment (7)), 1 as mutez);
  assert (Test.get_storage(taddr) == initial_storage + 7);
};

let test = originate_and_test(main);
```

</Syntax>

For performing mutation testing as before, we write the following test:

<Syntax syntax="pascaligo">

```pascaligo skip
const test_mutation =
  case Test.mutation_test(main, originate_and_test) of
    None -> unit
  | Some (_, mutation) -> block { Test.log(mutation) }
                          with failwith("Some mutation also passes the tests! ^^")
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let test_mutation =
  match Test.mutation_test main originate_and_test with
    None -> ()
  | Some (_, mutation) -> let () = Test.log(mutation) in
                          failwith "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let test_mutation =
  switch(Test.mutation_test(main, originate_and_test)) {
  | None => ()
  | Some (_, mutation) => { Test.log(mutation);
                            failwith ("Some mutation also passes the tests! ^^") }
  };
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let test_mutation =
  match(Test.mutation_test(main, originate_and_test), {
    None: () => unit,
    Some: pmutation => { Test.log(pmutation[1]);
                         failwith ("Some mutation also passes the tests! ^^") }
  });
```

</Syntax>

Running this test, the following output is obtained:

<Syntax syntax="pascaligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation-contract.ligo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation-contract.ligo", line 14, characters 2-15:
//  13 | function sub (const store : storage; const delta : int) : storage is
//  14 |   store - delta
//  15 |

// Replacing by: ADD(store ,
// delta).
// File "gitlab-pages/docs/advanced/src/mutation-contract.ligo", line 42, characters 31-82:
//  41 |   | Some (_, mutation) -> block { Test.log(mutation) }
//  42 |                           with failwith("Some mutation also passes the tests! ^^")
//  43 |   end

// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation-contract.mligo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation-contract.mligo", line 12, characters 51-64:
//  11 | let add (store, delta : storage * int) : storage = store + delta
//  12 | let sub (store, delta : storage * int) : storage = store - delta
//  13 |
//
// Replacing by: ADD(store ,
// delta).
// File "gitlab-pages/docs/advanced/src/mutation-contract.mligo", line 34, character 26 to line 35, character 76:
//  33 |     None -> ()
//  34 |   | Some (_, mutation) -> let () = Test.log(mutation) in
//  35 |                           failwith "Some mutation also passes the tests! ^^"
//
// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
$ ligo run test gitlab-pages/docs/advanced/src/mutation-contract.religo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation-contract.religo", line 12, characters 57-70:
//  11 | let add = ((store, delta) : (storage, int)) : storage => store + delta;
//  12 | let sub = ((store, delta) : (storage, int)) : storage => store - delta;
//  13 |
//
// Replacing by: ADD(store ,
// delta).
// File "gitlab-pages/docs/advanced/src/mutation-contract.religo", line 36, characters 28-80:
//  35 |   | Some (_, mutation) => { Test.log(mutation);
//  36 |                             failwith ("Some mutation also passes the tests! ^^") }
//  37 |   };
//
// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="jsligo">

```shell
ligo run test gitlab-pages/docs/advanced/src/mutation-contract.jsligo
// Outputs:
// Mutation at: File "gitlab-pages/docs/advanced/src/mutation-contract.jsligo", line 12, characters 55-68:
//  11 | let add = ([store, delta]: [storage, int]): storage => store + delta;
//  12 | let sub = ([store, delta]: [storage, int]): storage => store - delta;
//  13 |
//
// Replacing by: ADD(store ,
// delta).
// File "gitlab-pages/docs/advanced/src/mutation-contract.jsligo", line 41, characters 25-77:
//  40 |     Some: pmutation => { Test.log(pmutation[1]);
//  41 |                          failwith ("Some mutation also passes the tests! ^^") }
//  42 |   });
//
// Test failed with "Some mutation also passes the tests! ^^"
```

</Syntax>

The mutation testing found that the operation `sub` (corresponding to
the entrypoint `Decrement`) can be changed with no consequences in the
test: we take this as a warning signalling that the test above does not
cover the `Decrement` entrypoint. We can fix this by adding a new call
to the `Decrement` entrypoint in the test above:

<Syntax syntax="pascaligo">

```pascaligo skip
function originate_and_test(const mainf : parameter * storage -> return) is
  block {
    const initial_storage = 5;
    const (taddr, _, _) = Test.originate(mainf, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(7), 1mutez);
    const _ = Test.transfer_to_contract_exn(contr, Decrement(3), 1mutez);
    const storage = Test.get_storage(taddr);
    assert (storage = initial_storage + 4);
  } with (unit);
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let originate_and_test (mainf : parameter * storage -> return) =
  let initial_storage = 7 in
  let (taddr, _, _) = Test.originate mainf initial_storage 0tez in
  let contr = Test.to_contract taddr in
  let () = Test.transfer_to_contract_exn contr (Increment (7)) 1mutez in
  let () = Test.transfer_to_contract_exn contr (Decrement (3)) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 4)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let originate_and_test = (mainf : (parameter, storage) => return) => {
  let initial_storage = 5;
  let (taddr, _, _) = Test.originate(mainf, initial_storage, 0tez);
  let contr = Test.to_contract(taddr);
  let _ = Test.transfer_to_contract_exn(contr, (Increment (7)), 1mutez);
  let _ = Test.transfer_to_contract_exn(contr, (Decrement (3)), 1mutez);
  assert (Test.get_storage(taddr) == initial_storage + 4)
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let originate_and_test = (mainf : ((p: parameter, s: storage) => return_)) : unit => {
  let initial_storage = 5 as int;
  let [taddr, _, _] = Test.originate(mainf, initial_storage, 0 as tez);
  let contr = Test.to_contract(taddr);
  let r = Test.transfer_to_contract_exn(contr, (Increment (7)), 1 as mutez);
  let r = Test.transfer_to_contract_exn(contr, (Decrement (3)), 1 as mutez);
  assert (Test.get_storage(taddr) == initial_storage + 4);
};
```

</Syntax>

Running the updated test, we see that this time no mutation on `sub`
will give the same result.

## Multiple mutations

There is an alternative version of `Test.mutation_test` that will
collect all mutants that make the passed function correctly terminate.
Its type is similar to that of `Test.mutation_test`, but instead of
returning an optional type, it returns a list:

<Syntax syntax="pascaligo">

```pascaligo skip
Test.mutation_test_all : 'a -> ('a -> 'b) -> list ('b * mutation)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
Test.mutation_test_all : 'a -> ('a -> 'b) -> ('b * mutation) list
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
Test.mutation_test_all : ('a, ('a -> 'b)) => list ('b, mutation)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
Test.mutation_test_all : (value: 'a, tester: ('a -> 'b)) => list <['b, mutation]>;
```

</Syntax>

The example above can be modified to collect first all mutants, and
then process the list:

<Syntax syntax="pascaligo">

```pascaligo skip
const test_mutation =
  case Test.mutation_test_all(main, originate_and_test) of
    nil -> unit
  | ms -> block {
      for m in list ms block {
        const (_, mutation) = m;
        const path = Test.save_mutation(".", mutation);
        Test.log("saved at:");
        Test.log(path)
      }
    } with failwith("Some mutation also passes the tests! ^^")
  end
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
let test_mutation =
  match Test.mutation_test_all main originate_and_test with
    [] -> ()
  | ms -> let () = List.iter (fun ((_, mutation) : unit * mutation) ->
                              let path = Test.save_mutation "." mutation in
                              let () = Test.log "saved at:" in
                              Test.log path) ms in
          failwith "Some mutation also passes the tests! ^^"
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
let test_mutation =
  switch(Test.mutation_test_all(main, originate_and_test)) {
  | [] => ()
  | ms => { List.iter ((((_, mutation) : (unit, mutation)) => {
                        let path = Test.save_mutation(".", mutation);
                        Test.log("saved at:");
                        Test.log(path);}), ms);
            failwith ("Some mutation also passes the tests! ^^") }
  };
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
let test_mutation =
  match(Test.mutation_test_all(main, originate_and_test), list([
    ([]: list<[unit, mutation]>) => unit,
    ([hd,...tl]: list<[unit, mutation]>) => {
                         let ms = [hd,...tl];
                         for (const m of ms) {
                           let [_, mutation] = m;
                           let path = Test.save_mutation(".", mutation);
                           Test.log("saved at:");
                           Test.log(path);
                         };
                         failwith ("Some mutation also passes the tests! ^^") }
  ]));
```

</Syntax>

In this case, the list of mutants is processed by saving each mutation
to a file with the help of:

<Syntax syntax="pascaligo">

```pascaligo skip
Test.save_mutation : string -> mutation -> option (string)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
Test.save_mutation : string -> mutation -> string option
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
Test.save_mutation : (string, mutation) => option (string)
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
Test.save_mutation : (path: string, mutation: mutation) => option <string>
```

</Syntax>

where the first argument represents the path where the mutation is to
be saved, and the second argument is the mutation. This function
returns an optional string, representing either: the name of the file
where the mutation was saved or a failure.

## Preventing mutation

In some cases, it might be a good idea to prevent mutation in certain
places. A good example of this can be an assertion that is checking
some invariant. To prevent such mutations, the attribute
`@no_mutation` can be used:

<Syntax syntax="pascaligo">

```pascaligo skip
// This is testnew.ligo
type storage is int

type parameter is
  Increment of int
| Decrement of int

type return is list (operation) * storage

// Two entrypoints
function add (const store : storage; const delta : int) : storage is
  store + delta
[@no_mutation] function sub (const store : storage; const delta : int) : storage is
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
function main (const action : parameter; const store : storage) : return is block {
  [@no_mutation] const _ = assert (0 = 0);
} with
 ((nil : list (operation)),    // No operations
  case action of
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
// This is mutation-contract.mligo
type storage = int

type parameter =
  Increment of int
| Decrement of int

type return = operation list * storage

// Two entrypoints
let add (store, delta : storage * int) : storage = store + delta
[@no_mutation] let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, store : parameter * storage) : return =
 [@no_mutation] let _ = assert (0 = 0) in
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n))
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
// This is mutation-contract.religo
type storage = int;

type parameter =
  Increment (int)
| Decrement (int);

type return = (list (operation), storage);

// Two entrypoints
let add = ((store, delta) : (storage, int)) : storage => store + delta;
[@no_mutation] let sub = ((store, delta) : (storage, int)) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ((action, store) : (parameter, storage)) : return => {
 [@no_mutation] let _ = assert (0 == 0);
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Increment (n) => add ((store, n))
  | Decrement (n) => sub ((store, n))}))
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
/ This is mutation-contract.jsligo
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int];

type return_ = [list<operation>, storage];

// Two entrypoints
let add = ([store, delta]: [storage, int]): storage => store + delta;
/* @no_mutation */ let sub = ([store, delta]: [storage, int]): storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store]: [parameter, storage]) : return_ => {
  /* @no_mutation */ let _ = assert (0 == 0);
  return [
    list([]) as list<operation>,    // No operations
    match(action, {
      Increment:(n: int) => add ([store, n]),
      Decrement:(n: int) => sub ([store, n])})
  ]
};
```

</Syntax>

In the example, two mutations are prevented. The first one, it is on
the function `sub`, which prevents the mutations presented in the
example from the previous sections. The second one, it is an assertion
of a silly invariant, `0` equals `0`, that should not be mutated to
things like: `0` less than `0`, `0` equal `1`, etc.
