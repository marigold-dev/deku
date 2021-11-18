---
id: michelson-and-ligo
title: Michelson and LIGO
---

Currently LIGO compiles to [Michelson](https://tezos.gitlab.io/whitedoc/michelson.html),
the native smart contract language supported by Tezos. This page explains the
relationship between LIGO and the underlying Michelson it compiles to. Understanding
Michelson is not a requirement to use LIGO, but it does become important if you want
to formally verify contracts using [Mi-Cho-Coq](https://gitlab.com/nomadic-labs/mi-cho-coq/)
or tune the performance of contracts outputted by the LIGO compiler.

**The rationale and design of Michelson**

Michelson is a Domain-Specific Language (DSL) for writing Tezos smart contracts
inspired by Lisp and Forth. This unusual lineage aims at satisfying unusual
constraints, but entails some tensions in the design.

First, to measure step-wise gas consumption, *Michelson is interpreted*.

On the one hand, to assess gas usage per instruction, instructions
should be simple, which points to low-level features (a RISC-like
language). On the other hand, it was originally thought that users
will want to write in Michelson instead of lowering a language to
Michelson, because the gas cost would otherwise be harder to
predict. This means that *high-level features* were deemed necessary
(like a restricted variant of Lisp lambdas, a way to encode algebraic
data types, as well as built-in sets, maps and lists).

To avoid ambiguous and otherwise misleading contracts, the layout of
Michelson contracts has been constrained (e.g., indentation, no
UTF-8), and a *canonical form* was designed and enforced when storing
contracts on the chain.

To reduce the size of the code, Michelson was designed as *a
stack-based language*, whence the lineage from Forth and other
concatenative languages like PostScript, Joy, Cat, Factor etc. (Java
byte-code would count too.)

Programs in those languages are *compact* because they assume an
implicit stack in which some input values are popped, and output
values are pushed, according to the current instruction being
executed.

*Each Michelson instruction modifies a prefix of the stack*, that is,
a segment starting at the top.

Whilst the types of Michelson instructions can be polymorphic, their
instantiations must be monomorphic, hence *Michelson instructions are
not first-class values* and cannot be partially interpreted.

This enables a simple *static type checking*, as opposed to a complex
type inference. It can be performed efficiently: *contract type
checking consumes gas*. Basically, type checking aims at validating
the composition of instructions, therefore is key to safely composing
contracts (concatenation, activations). Once a contract passes type
checking, it cannot fail due to inconsistent assumptions on the
storage and other values (there are no null values, no casts), but it
can still fail for other reasons: division by zero, token exhaustion,
gas exhaustion, or an explicit `FAILWITH` instruction. This property
is called *type safety*. Also, such a contract cannot remain stuck:
this is the *progress property*.

The existence of a formal type system for Michelson, of a formal
specification of its dynamic semantics (evaluation), of a Michelson
interpreter in Coq, of proofs in Coq of properties of some typical
contracts, all those achievements are instances of *formal methods in
Tezos*.

Here is an example of a Michelson contract.

**`counter.tz`**
```text
{ parameter (or (or (nat %add) (nat %sub)) (unit %default)) ;
  storage int ;
  code { AMOUNT ; PUSH mutez 0 ; ASSERT_CMPEQ ; UNPAIR ;
         IF_LEFT
           { IF_LEFT { ADD } { SWAP ; SUB } }
           { DROP ; DROP ; PUSH int 0 } ;
         NIL operation ; PAIR } }
```

The contract above maintains an `int` as its storage. It has two
[entrypoints](https://tezos.gitlab.io/whitedoc/michelson.html#entrypoints),
`add` and `sub`, to modify it, and the `default` entrypoint of type
`unit` will reset it to `0`.

The contract itself contains three sections:
- `parameter` - The argument provided by a transaction invoking the contract.
- `storage` - The type definition for the contract's data storage.
- `code` - Actual Michelson code that has the provided parameter and
  the current storage value in its initial stack. It outputs in the
  resulting stack a pair made of a list of operations and a new
  storage value.

Michelson code consists of *instructions* like `IF_LEFT`, `PUSH ...`,
`UNPAIR` etc. that are composed sequentially in what is called a
*sequence*. The implicit stack contains at all times the state of the
evaluation of the program, whilst the storage represents the
persistent state. If the contract execution is successful, the new
storage state will be committed to the chain and become visible to all
the nodes. Instructions are used to transform a prefix of the stack,
that is, the topmost part of it, for example, by duplicating its top
element, dropping it, subtracting the first two etc.

> 💡 A Michelson program running on the Tezos blockchain is meant to
> output a pair of values including a `list of operations` to include
> in a transaction, and a new `storage` value to persist on the chain.

## Stack versus variables

Perhaps the biggest challenge when programming in Michelson is the
lack of *variables* to denote the data: the stack layout has to be
kept in mind when retrieving and storing data. For example, let us
implement a program in JavaScript that is similar to the Michelson
above:

**`counter.js`**
```javascript
var storage = 0;

function add (a) { storage += a; }
function sub (a) { storage -= a; }

// We are calling this function "reset" instead of "default"
// because `default` is a Javascript keyword

function reset () { storage = 0; }
```

In our JavaScript program the initial `storage` value is `0` and it
can be modified by calling `add (a)`, `sub (a)` and `reset ()`.

We cannot run JavaScript on the Tezos blockchain, but we can choose
LIGO, which will abstract the stack management and allow us to create
readable, type-safe, and efficient smart contracts.

