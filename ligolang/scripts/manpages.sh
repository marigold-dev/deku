#!/usr/bin/env nix-shell
#!nix-shell -p pandoc -i bash

# shellcheck shell=bash

set -euET -o pipefail

cd "$( dirname "${BASH_SOURCE[0]}" )"/../gitlab-pages/docs/manpages

opam exec -- dune exec -- ligo --help=groff | perl -pe "s/\\\\N'39'/chr(96)/ge" | perl -pe "s/\\\\N'(\d+)'/chr(\$1)/ge" | pandoc -f man -o ligo.md --shift-heading-level-by=2

for SUBCOMMAND in "test" "measure-contract" "compile-contract" "compile-parameter" "compile-storage" "compile-expression" "transpile-contract" "transpile-expression" "interpret" "dry-run" "evaluate-call" "evaluate-expr" "changelog" "print-graph" "print-cst" "print-ast" "print-ast-sugar" "print-ast-core" "print-ast-typed" "print-ast-combined" "print-mini-c" "list-declarations" "preprocess" "pretty-print" "get-scope"; do
  opam exec -- dune exec -- ligo $SUBCOMMAND --help=groff | perl -pe "s/\\\\N'39'/chr(96)/ge" | perl -pe "s/\\\\N'(\d+)'/chr(\$1)/ge" | pandoc -f man -o $SUBCOMMAND.md --shift-heading-level-by=2;
done
