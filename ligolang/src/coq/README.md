## Ligo_coq

This directory contains Coq code implementing part of the "backend"
compiler.

Extraction is performed in
[extraction.v](../coq_ocaml/extraction.v). This results in a module
`Ligo_coq_ocaml` which contains the various modules extracted from
Coq, as listed in the [dune file](../coq_ocaml/dune).

## Development

To develop Coq here, you could simply edit .v files and `dune build`
(or `nix build ...`.) But this is not very nice.

You can use Proof General like so:

```sh
# prepare to use PG:
cd src/coq
make
# ...use PG...
# prepare to use dune again:
make clean
dune build
```

(Dune will complain if you don't `make clean` the PG files out.)

## Overview

At the moment, only pass 14-stacking is implemented in
Coq. `Ligo_coq_ocaml.Compiler` is integrated into the rest of the LIGO
compiler in
[15-stacking/compiler_program.ml](../passes/15-stacking/compiler_program.ml). Certain
hypotheses in the Coq compiler are instantiated there, too.

An intermediate language is defined in [ligo.v](./ligo.v). This is
emitted by the previous pass in
[14-scoping/scoping.ml](../passes/14-scoping/scoping.ml).

In [micheline.v](./micheline.v) a Coq version Micheline is defined
which extracts directly to Tezos_micheline.

This Micheline is used for source (IR) types in [ligo.v](./ligo.v) and
target (Michelson) types and programs in
[michelson.v](./michelson.v). This allows us to use the identity as
the type translation function for the compiler.

The main compiler function is `compile_expr` in
[compiler.v](./compiler.v), which translates the IR expressions to
Michelson. (We need to optimise the resulting Micheline still, but
that is not in Coq, yet.)

The only current proof is a type preservation proof, stated at
`expr_type_preservation` and proved at `type_preservation`, in
[compiler.v](./compiler.v).
