
## What

A language server for all three dialects of LIGO.

## Design choices

The server contains 2 big parts:
1) Grammars
2) Server itself.

Grammars are in separate folders, in form of tree-sitter grammars.
They are compiled via `tree-sitter generate` command in their respective folder.

Then they are linked as `parser.c` file with GHC toolchain.
Because GHCi REPL is unable to link from outside of project folder, the `parser.c` is
symlinked into `vendor/` directory.

Right now only one parser is linked there; the name of symlink should be changed
in future.

Server part invokes the TS-parser and then constructs a tree of haskell datatypes
from the TS-tree. Then it is fed into `LIGO.Parser` (name will be changed), which
constructs a universal LIGO from the tree of the respective language.

Parser uses the root `Parser` module which provides the combinators and monad
to deal with `ParseTree`, produced from TS output.

The tree it produces has:
1) An `ASTInfo` at each node, which contains the `Range` of that node and
   preceeding comments. Sadly, but the comments after all the code in the module
   are lost.
2) Some structure;
3) Ability to be just an `Error` instead of all above. An error contains `Range`
   as well.

It is possible to update the scopes in the tree, using `updateTree` function,
which is a specialised custom `traverse`.

It is also possible to get the zipper-like chain of trees, covering some node,
in ascending order - using `spineTo` function. If you have already pinned
the scopes, you can just take first one and pull the scope out of it. Unless
it is an `Error` node. We probably need error nodes to be `Functor`s
as well as any other structures used to build the tree.

On top of all that is an event loop for handling messages from some lsp-client.

## For developers

To compile and run, the following tools are needed:

1) tree-sitter-cli@0.19.5 (the Node.js one was used during development)
2) optionally, nix package manager
3) haskell-stack (preferably version `2.5.1`)

First, you need to generate the `parser.c` for all dialects.
For that, do

```
cd tools/lsp/squirrel/grammars && make
```

`squirrel` package also has appropriate `Makefile` for development, so running `make`
would build `squirrel` project
