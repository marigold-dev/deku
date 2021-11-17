---
title: Public Launch of LIGO
author: Gabriel Alfour
---

# Public Launch of [LIGO](https://ligolang.org/)

---

## A Refresher: What is LIGO?
LIGO is a statically typed high-level smart contract language that compiles down to Michelson. It seeks to be easy to use, extensible and safe.

The core language is being developed by The Marigold Project. George Dupéron and Christian Rinderknecht of Nomadic Labs help on the core language, and tooling for LIGO is being developed by Stove Labs (Granary, docs and infrastructure) and Brice Aldrich (syntax highlighting). 

Our previous Medium posts about LIGO can be found [here](https://medium.com/tezos/introducing-ligo-a-new-smart-contract-language-for-tezos-233fa17f21c7) and [here](https://medium.com/tezos/ligo-becomes-polyglot-a474e2cb0c24).

## The State of LIGO
Today, we are publicly releasing LIGO in beta\*. We've focused on making the onboarding process for LIGO as painless as possible and encourage you to check out our [tutorials](/docs/tutorials/get-started/tezos-taco-shop-smart-contract) and [documentation](https://ligolang.org/docs/next/intro/installation).

We are fixing bugs and adding features to LIGO (e.g. some Michelson primitives like iterators are missing) by the day. Please submit issues about bugs and missing features you need when you encounter them, and you just might find those solved in the following week.

We have been also working to extend the capabilities of Michelson, benefitting all languages (e.g. SmartPy) in the Tezos ecosystem. These proposed changes include adding multiple entrypoints, partial application (enabling cheap closures) and new operators for fast stack access to Michelson. We will submit these improvements with Nomadic Labs and Cryptium in an amendment planned for the next proposal period.

## Sample Contract

Here are two samples equivalent contracts written in two different syntaxes. They add or substract an amount to the storage depending on the parameter.

```pascal
// Pascaligo syntax
type action is
| Increment of int
| Decrement of int

function main (const p : action ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : list(operation)),
    case p of
    | Increment(n) -> s + n
    | Decrement(n) -> s - n
    end)
```

```ocaml
(* Cameligo syntax *)
type action =
| Increment of int
| Decrement of int

let main (p : action) (s : int) : (operation list * int) =
  let storage =	
    match p with
    | Increment n -> s + n
    | Decrement n -> s - n in
  (([] : operation list) , storage)
```

## Roadmap

### Short-Term
#### June 2019
<span style="display:block">✓ First public release (hi)</span>
<span style="display:block">✓ PascaLIGO and CameLIGO</span>
<span style="display:block">✓ Docs</span>
<span style="display:block">✓ Tutorials</span>
<span style="display:block">\- Integration testing in JS/Reason with [Granary](https://stove-labs.github.io/granary/)</span>

#### July 2019
<span style="display:block">\- Try ligo online</span>
<span style="display:block">\- Unit testing in LIGO</span>
<span style="display:block">\- ReasonLIGO (ReasonML syntax)</span>
<span style="display:block">\- Design Pattern repository</span>

### Mid-Term
We are currently planning 3 big projects on the core language (excluding tooling).

#### Generic Front End (GFE)
The PascaLIGO and CameLIGO parsers, pretty-printers and highlighters were written by hand. The same will be done for the ReasonML syntax in July. 
The Generic Front End is a project to alleviate the need to do this manually for future syntaxes. The idea of the GFE is to develop a system that can take in a syntax description, and then generate:
- A parser
- A displayer
- A transpiler between syntaxes
- A syntax highlighter
- Some documentation

(A prototoype can be found in the code base that generated a PrettyPrinter, a Parser and an AST.)

#### Super Type System (STS)
The current type system is very basic: it is structural, non-polymorphic, without subtyping, names, references, advanced inference or effects. We are planning to change that.
We are looking to develop a Super Type System that has the following features:
- A rich type system. We are planning to integrate standard features (polymorphism, names), clear error messages and intuitive type inference.
- An effect system. This is important to capture failure cases, write effects in an idiomatic yet safe style (rather than passing around the storage through function calls) or capture which contracts can be called.
- An easy-to-use API. We want people to easily build static analysis tools on top of LIGO.

#### Real-time Benchmark

The current version explicitly excludes non-essential features which can produce unexpected explosions in gas costs. To alleviate this constraint, we plan to integrate gas benchmarks on all top-level declarations with some fuzzing. This will allow developers and users to estimate the cost of their contracts in real time.

## Getting Started and Contact
Come visit [our website](https://ligolang.org)! You can also join our [Discord](https://discord.gg/9rhYaEt), Riot (*#ligo-public:matrix.org*) or [Telegram Chat](https://t.me/LigoLang).



\* Following software release cycle conventions, it should be called a pre-alpha. But most people don't know the difference.
