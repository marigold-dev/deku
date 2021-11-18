---
id: vendors
title: Vendors
---

Next to LIGO’s main pipeline, we use some other libraries, that are in the folder `vendors`.
## ligo-utils
This, quite expectedly, defines utilities that are used in LIGO.
There are three kinds of utilities, corresponding to their dependencies.

`tezos-utils` contain utilities that depend on some Tezos libraries.

`proto-alpha-utils` contain utilities that depend on the compilation of some Tezos protocol. It is very big and thus can’t be compiled in JS. This is because of a dependency to this that we don’t have LIGO in the browser yet.

`simple-utils` contain most utilities. For instance, in `simple-utils` are `X_list` and `X_option`, that extend the `List` module and the `option` type found in `Pervasives`. 

Of particular interest in `simple-utils` is `trace.ml`, a module used pervasively in LIGO’s code-base. It deals with exceptions (and soon enough debugging annotations) in a functional manner. It offers a lot of utilities to build, combine and propagate exceptions. Given it’s used everywhere, that it relies on some advanced features of OCaml (higher order functions, ppx preprocessing) and that it exposes **a lot** of functions, it’s a good idea to look at this file’s documentation.
## tezos-modded
`tezos-modded` is a modded version of Tezos. There are modifications in it that are quite useful (exposing more functions from the protocol, giving more options to functions already defined), but not integrated yet.
It should in the end be integrated into the Tezos protocol, but until then, we use this.

