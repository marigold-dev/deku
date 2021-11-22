---
id: back-end
title: Back End
---

The Back-End is the part that compiles down to Michelson. Instead of a single compilation step, it is separated in two parts.
## Transpiler and Mini_C
The Transpiler is a function that takes as input the Typed AST, and outputs expressions in a language that is basically a Michelson based on with named variables and first-class-environments.
On the one hand, there are cases in the AST like `E_if_bool` or `E_make_empty_list` that would be directly translated in Michelson like `IF {} {}` or `NIL`.
On the other hand, there are cases in the AST like `E_variable` or `E_environment_select` that specifically target the compiler.
The files of the Transpiler are in `transpiler/`, while those of Mini_c are in `mini_c/`.
## Compiler
The previous LIGO’s compilation to Michelson model was quite complicated. The current one is quite straightforward, where the environment of variables (x -> 12, y -> “foo”) is compiled as Michelson stack (12 :: foo).
It has been simplified for multiple reasons:
Having a simple model reduces its number of points of failure.
Having a simple model makes optimising it easier.
We submitted a change to the Tezos’ protocol that actually make it more efficient.

