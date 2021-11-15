---
id: introduction
title: Introduction to LIGO
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

LIGO is a programming language for writing [Tezos](https://tezos.com/) smart contracts.
Smart contracts are a unique domain with extreme resource constraints and even
more extreme security risks. Unlike desktop, mobile, or web
application development, smart contracts cannot rely on cheap CPU time and memory.
All resources used by contracts are expensive, and tracked as 'gas costs'. Smart
contracts often directly control money or assets, which if stolen could rack up to
a large financial loss to the contracts controllers and users. Tezos smart contracts
live on the blockchain forever, if there's a bug in them they can't be patched or
amended. Naturally, under these conditions, it's not possible to develop smart contracts
the way we're used to developing user-facing applications.

LIGO is designed with these problems in mind. The design philosophy can be
described in a few bullet points:

1. Make a clean, simple language with no unnecessary parts.

2. Offer multiple familiar syntaxes so users can get up and running quickly.

3. Encourage people to write simple code, so that it's easy to formally verify the
compiled output using a project like [Mi-Cho-Coq](https://gitlab.com/nomadic-labs/mi-cho-coq/).

4. Significantly reduce the risk that your smart contract will lose its balance to an [avoidable exploit](https://www.wired.com/2016/06/50-million-hack-just-showed-dao-human/).

LIGO is a functional language designed to include the features you need while
avoiding patterns that make formal verification hard. Most useful smart contracts
can express their core functionality in under a thousand lines of code. This makes
them a good target for formal methods, and what can't be easily proven can at least
be extensively tested. The simplicity of LIGO also keeps its compiled output
unbloated. Our hope is to have a simple, strongly typed language with a low footprint.

LIGO currently offers three syntaxes:

  - **PascaLIGO**, a syntax inspired by Pascal which provides an
    imperative developer experience.

  - **CameLIGO**, an [OCaml](https://ocaml.org/) inspired
    syntax that allows you to write in a functional style.

  - **ReasonLIGO**, a [ReasonML](https://reasonml.github.io/) inspired syntax
    that builds on the strong points of OCaml. It aims to be familiar to those
    coming from JavaScript.

Let's define some LIGO contract in the three flavours above. Do
not worry if it is a little confusing at first; we will explain all
the syntax in the upcoming sections of the documentation.


<Tabs
  defaultValue="pascaligo"
  values={[
    { label: 'PascaLIGO', value: 'pascaligo', },
    { label: 'CameLIGO', value: 'cameligo', },
    { label: 'ReasonLIGO', value: 'reasonligo', },
    { label: 'JsLIGO', value: 'jsligo', },
  ]
}>
<TabItem value="pascaligo">

```pascaligo group=a
type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

type return is list (operation) * storage

function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),
  case action of
    Increment (n) -> store + n
  | Decrement (n) -> store - n
  | Reset         -> 0
 end)
```

</TabItem>
<TabItem value="cameligo">

```cameligo group=a
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

let main (action, store : parameter * storage) : return =
  ([] : operation list),
  (match action with
     Increment n -> store + n
   | Decrement n -> store - n
   | Reset       -> 0)
```

</TabItem>
<TabItem value="reasonligo">

```reasonligo group=a
type storage = int;

type parameter =
  Increment (int)
| Decrement (int)
| Reset;

type return = (list (operation), storage);

let main = ((action, store): (parameter, storage)) : return => {
  (([] : list (operation)),
  (switch (action) {
   | Increment (n) => store + n
   | Decrement (n) => store - n
   | Reset         => 0}));
};
```

</TabItem>
<TabItem value="jsligo">

```jsligo group=a
type storage = int;

type parameter =
  ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list<operation>, storage];

let main = ([action, store]: [parameter, storage]) : return_ => {
  return [
    list([]) as list<operation>,
    match(action, {
      Increment: (n: int) => store + n,
      Decrement: (n: int) => store - n,
      Reset:     ()       => 0
    })
  ];
};
```

</TabItem>
</Tabs>

This LIGO contract accepts the following LIGO expressions:
`Increment(n)`, `Decrement(n)` and `Reset`. Those serve as
`entrypoint` identification.

---

## Runnable code snippets

Some of the sections in this documentation will include runnable code snippets. Sources for those are available at
the [LIGO GitLab repository](https://gitlab.com/ligolang/ligo).

### Snippets

For example **code snippets** for the *Types* subsection of this doc, can be found here:
`gitlab-pages/docs/language-basics/src/types/**`

### Running snippets

In certain cases it makes sense to be able to run/evaluate the given snippet. Usually there will be an example command which you can use, such as:

```shell
ligo run evaluate-expr gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo --entry-point age -s pascaligo
# Outputs: 25
```
