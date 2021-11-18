---
id: michelson_testing
title: Testing Michelson code
---

import Syntax from '@theme/Syntax';

## Testing Michelson code

There are multiple frameworks for testing Michelson contracts, we will
not get into details, but here is a list of tutorials showing how to
test contracts in Michelson:

* [PyTezos](https://baking-bad.org/blog/2019/09/16/testing-michelson-tezos-contracts-with-pytezos-library/)

* [Cleveland](https://gitlab.com/morley-framework/morley/-/blob/9455cd384b2ab897fb7b31822abca3730a4ad08b/code/cleveland/testingEDSL.md)

Another alternative is to use Tezos's binary `tezos-client`
directly. There's a new
[mockup](https://tezos.gitlab.io/user/mockup.html) mode which is does
not need a Tezos node to be running (albeit this is less similar to
mainnet than running a Tezos sandboxed node).

### Testing with `tezos-client`'s mockup

We show the main steps that need to be done to use the mockup mode to
test our LIGO contracts. As a first step, we need to compile our LIGO
contract to Michelson code. Suppose we write the following simple
contract:

<Syntax syntax="pascaligo">

```pascaligo
// This is mockup_testme.ligo
type storage is string

type parameter is
  Append of string

type return is list (operation) * storage

function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Append (s) -> store ^ s
  end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
// This is mockup_testme.mligo
type storage = string

type parameter =
  Append of string

type return = operation list * storage

let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Append (s) -> store ^ s)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
// This is mockup_testme.religo
type storage = string;

type parameter =
  Append (string)

type return = (list (operation), storage);

let main = ((action, store) : (parameter, storage)) : return => {
 (([] : list (operation)),    // No operations
 (switch (action) {
  | Append (s) => store ++ s
  }))
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
// This is mockup_testme.jsligo
type storage = string;

type parameter =
| ["Append", string];

type return_ = [list<operation>, storage];

let main = ([action, store]: [parameter, storage]): return_ => {
 return [list([]) as list<operation>,    // No operations
  match(action, {
    Append: (s: string) => store + s
  })]
};
```

</Syntax>

To obtain Michelson code from it, we run the LIGO compiler:

<Syntax syntax="pascaligo">

```shell
ligo compile contract gitlab-pages/docs/advanced/src/testing/mockup_testme.ligo --entry-point main
// Outputs:
// { parameter string ;
//   storage string ;
//   code { UNPAIR ; SWAP ; CONCAT ; NIL operation ; PAIR } }
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/advanced/src/testing/mockup_testme.mligo --entry-point main
// Outputs:
// { parameter string ;
//   storage string ;
//   code { DUP ; CAR ; SWAP ; CDR ; CONCAT ; NIL operation ; PAIR } }
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile contract gitlab-pages/docs/advanced/src/testing/mockup_testme.religo --entry-point main
// Outputs:
// { parameter string ;
//   storage string ;
//   code { DUP ; CAR ; SWAP ; CDR ; CONCAT ; NIL operation ; PAIR } }
```

</Syntax>

Instead of outputting the resulted compiled code in the screen, we can
tell LIGO to write it in a file called `mockup_testme.tz`:

<Syntax syntax="pascaligo">

```shell
ligo compile contract gitlab-pages/docs/advanced/src/testing/mockup_testme.ligo --entry-point main --output-file mockup_testme.tz
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile contract gitlab-pages/docs/advanced/src/testing/mockup_testme.mligo --entry-point main --output-file mockup_testme.tz
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile contract gitlab-pages/docs/advanced/src/testing/mockup_testme.religo --entry-point main --output-file mockup_testme.tz
```

</Syntax>


Now it is time to test this Michelson code we obtained: we want to
execute it using the mockup mode.

Before anything, make sure you have installed `tezos-client`, a simple
way to do so is by using opam (`opam install tezos-client`).

We can list all the protocols available using `tezos-client list
mockup protocols`. In this example, we will use Edo for testing, so
the command we use for creating a mockup instance on the directory
`/tmp/mockup/` is:

```shell
tezos-client \
  --protocol PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq \
  --base-dir /tmp/mockup \
  --mode mockup \
  create mockup
```

This command returns a list of Tezos addresses that we can use with
the client in subsequent commands. As recommended in the Tezos
documentation, we can add a shell alias to avoid mistakes:

```shell
alias mockup-client='tezos-client --mode mockup --base-dir /tmp/mockup'
```

We can list the addresses returned above by running:
```shell
mockup-client list known addresses
// Outputs:
// bootstrap5: tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv (unencrypted sk known)
// bootstrap4: tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv (unencrypted sk known)
// bootstrap3: tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU (unencrypted sk known)
// bootstrap2: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN (unencrypted sk known)
// bootstrap1: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx (unencrypted sk known)
```

We are now ready to originate (or "deploy") the contract on our mockup
Tezos:

```shell
mockup-client originate contract mockup_testme \
              transferring 0 from bootstrap1 \
              running "`cat mockup_testme.tz`" \
              --init \"foo\" --burn-cap 0.1
```

The `--init` argument (`"foo"`) is the initial storage for our
deployed contract. In case we had a more complex storage, we could
have used LIGO's `compile-storage` sub-command to compile a LIGO
expression to a Michelson storage.

Now it is time to test! The property we want to check is that if we
execute `Append ("bar")` on our contract with storage `"foo"`, then
the contract updates its storage to `"foobar"`.

As a first sanity check, we can confirm that the storage is currently `"foo"`:

```shell
mockup-client get contract storage for mockup_testme
// Outputs:
// "foo"
```

Then, we execute a call to our contract with parameter `Append
("bar")`. To do so, we first compile the parameter as follows:

<Syntax syntax="pascaligo">

```shell
ligo compile parameter gitlab-pages/docs/advanced/src/testing/mockup_testme.ligo "Append (\"bar\")" --entry-point main
// Outputs:
// "bar"
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile parameter gitlab-pages/docs/advanced/src/testing/mockup_testme.mligo "Append (\"bar\")" --entry-point main
// Outputs:
// "bar"
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile parameter gitlab-pages/docs/advanced/src/testing/mockup_testme.religo "Append (\"bar\")" --entry-point main
// Outputs:
// "bar"
```

</Syntax>

So our parameter is simply the string (notice that the constructor
`Append` was removed). We execute a call to the contract with this
compiled parameter as follows:

```shell
mockup-client transfer 0 from bootstrap2 \
              to mockup_testme \
              --arg \"bar\" --burn-cap 0.01
```

We have chosen `bootstrap2` as the origin of this call (for no
particular reason, any address could do).

We can finally check that that our property holds: the storage is now
"foobar":

```shell
mockup-client get contract storage for mockup_testme
// Outputs:
// "foobar"
```

Good! Our contract passed the test successfully!
