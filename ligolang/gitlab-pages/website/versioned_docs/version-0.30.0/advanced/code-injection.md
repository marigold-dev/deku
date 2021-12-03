---
id: embedded-michelson
title: Embedded Michelson
---

import Syntax from '@theme/Syntax';

If you have an existing piece of Michelson code that you want to use
as-is, LIGO provides the ability to embed Michelson code. This feature
can be useful when you need to have a deep level of control over the
generated code, for example for optimisation, or if you need to use a
feature from Michelson that is not yet supported by high-level
constructions in LIGO.

## Embedding Code

The syntax for embedding Michelson is by means of the `[%Michelson
...]` construction. The ellipsis is meant to denote an annotated
string literal containing the Michelson code to be injected in the
generated Michelson and the type (as a function) of the Michelson
code.

<Syntax syntax="pascaligo">

```pascaligo
  function michelson_add (var n : nat * nat ) : nat is block {
    const f : (nat * nat -> nat) =
      [%Michelson ({| { UNPAIR ; ADD } |} : nat *nat -> nat)];
  } with f (n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let michelson_add (n : nat * nat) : nat =
  [%Michelson ({| { UNPAIR ; ADD } |} : nat * nat -> nat) ] n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let michelson_add = (n : (nat, nat)) : nat =>
  [%Michelson ({| { UNPAIR ; ADD } |} : ((nat, nat) => nat)) ](n);
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let michelson_add = (n: [nat, nat]): nat =>
  (Michelson`{ UNPAIR ; ADD }` as ((n: [nat, nat]) => nat))(n);
```

</Syntax>

Note that the type annotation is required, because the embedded Michelson code
is not type checked by LIGO. This assumes that the given type is correct.

In the example above, the notation ```{| ... |}``` is used to
represent a verbatim string literal, that is, an uninterpreted string,
which here contains a piece of Michelson code. The type annotation
describes the behaviour of the Michelson code:

- It starts working on a stack consisting of a tuple of `nat`s: `[ nat * nat ]`.

- The tuple is destructured using `UNPAIR`: `[ nat ] [ nat ]`.

- The two top values of the stack are added using `ADD`,
  and stops working on a stack consisting of a single `nat`: `[ nat ]`.

The compiler will prevent changes to the embedded Michelson code if
the function resulting from the embedded code is not applied. For
example, let's see what happens when we compile an embedded Michelson
expression that pushes some value on the stack, then drops it
immediately, and then continues as a regular increment function.

<Syntax syntax="pascaligo">

```shell
ligo compile expression pascaligo "[%Michelson ({| { PUSH nat 42; DROP ; PUSH nat 1; ADD } |} : nat -> nat)]"
// Outputs:
// { PUSH nat 42 ; DROP ; PUSH nat 1 ; ADD }
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile expression cameligo "[%Michelson ({| { PUSH nat 42; DROP ; PUSH nat 1; ADD } |} : nat -> nat)]"
// Outputs:
// { PUSH nat 42 ; DROP ; PUSH nat 1 ; ADD }
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile expression reasonligo "[%Michelson ({| { PUSH nat 42; DROP ; PUSH nat 1; ADD } |} : (nat => nat))]"
// Outputs:
// { PUSH nat 42 ; DROP ; PUSH nat 1 ; ADD }
```

</Syntax>

As we can see, the embedded Michelson code was not modified. However,
if the resulting function is applied, then the embedded Michelson code
could be modified/optimised by the compiler. To exemplify this
behaviour, an application can be introduced in the example above by
eta-expanding. In this case, the first two instructions will be
removed by LIGO because they have no effect on the final result.

<Syntax syntax="pascaligo">

```shell
ligo compile expression pascaligo "function (const n : nat) : nat is ([%Michelson ({| { PUSH nat 42; DROP ; PUSH nat 1; ADD } |} : nat -> nat)])(n)"
// Outputs:
// { PUSH nat 1 ; ADD }
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile expression cameligo "fun (n : nat) -> [%Michelson ({| { PUSH nat 42; DROP ; PUSH nat 1; ADD } |} : nat -> nat)] n"
// Outputs:
// { PUSH nat 1 ; ADD }
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile expression reasonligo "((n : nat) => [%Michelson ({| { PUSH nat 42; DROP ; PUSH nat 1; ADD } |} : (nat => nat))](n))"
// Outputs:
// { PUSH nat 1 ; ADD }
```

</Syntax>

## Compiling Embedded Code

Contracts with embedded Michelson code are compiled normally like any
other contract. We give an example of a contract that uses the type
`never`, a new Michelson type that represents the empty type. You can
read more about it
[here](https://tezos.gitlab.io/008/michelson.html#operations-on-type-never).

We will use the Michelson instruction `NEVER` to resolve a forbidden
branch when matching on the parameter of our contract:

<Syntax syntax="pascaligo">

```pascaligo skip
type parameter is
  Increment of int
| Extend of never

type storage is int

function main(const action : parameter; const store : storage) : list (operation) * storage is
  ((nil : list (operation)),
   case action of
     Increment (n) -> store + n
   | Extend (k) -> block {
       const f : (never -> int) =
        [%Michelson ({| { NEVER } |} : never -> int)];
     } with f (k)
   end)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo skip
type parameter =
  Increment of int
| Extend of never

type storage = int

let main(action, store : parameter * storage) : operation list * storage =
  ([] : operation list),
  (match action with
    Increment n -> store + n
  | Extend k -> [%Michelson ({| { NEVER } |} : never -> int)] k)
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo skip
type parameter =
| Increment (int)
| Extend (never);

type storage = int;

let main = ((action,store): (parameter, storage)) => {
  let storage =
    switch (action) {
    | Increment (n) => store + n
    | Extend (k) => [%Michelson ({| { NEVER } |} : (never => int))](k)
    };
  ([]: list(operation), storage);
};
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo skip
type parameter =
  ["Increment", int]
| ["Extend", never];

type storage = int;

let main = ([action,store]: [parameter, storage]) => {
  let storage =
    match(action, {
     Increment: (n: int) => store + n,
     Extend: (k: never) => (Michelson`{ NEVER }` as ((n: never) => int))(k);
    });
  return [list([]) as list<operation>, storage];
};
```

</Syntax>

Assuming we have saved those contents in a file with name `never`, we
can compile it using the following command:

<Syntax syntax="pascaligo">

```shell
ligo compile contract --protocol edo --disable-michelson-typechecking gitlab-pages/docs/advanced/src/code-injection/never.ligo --entry-point main
```

</Syntax>
<Syntax syntax="cameligo">

```shell
ligo compile contract --protocol edo --disable-michelson-typechecking gitlab-pages/docs/advanced/src/code-injection/never.mligo --entry-point main
```

</Syntax>
<Syntax syntax="reasonligo">

```shell
ligo compile contract --protocol edo --disable-michelson-typechecking gitlab-pages/docs/advanced/src/code-injection/never.religo --entry-point main
```

</Syntax>

> ⚠️ Just for reference, there is support now for generating the
> instruction `NEVER` directly from LIGO, using `Tezos.never`.
