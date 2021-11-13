# Stages and passes in order:

Passes operating on a stage are listed under that stage. The last pass
emits the next stage.

Emacs tip: markdown-mode binds C-c C-o (also middle mouse button) to
markdown-follow-thing-at-point which will follow these links to dired
buffers.

- Text (CameLIGO, ReasonLIGO, PascaLIGO)
  - [Preprocessing](./passes/00-preprocessing)
  - [Lexing](./passes/01-lexing)
  - [Parsing](./passes/02-parsing)
- [CST](./stages/1-cst) Concrete syntax
  - [Self\_cst](./passes/03-self_cst)
  - [Tree\_abstraction](./passes/04-tree_abstraction)
- [Ast\_imperative](./stages/2-ast_imperative)
  - [Self\_ast\_imperative](./passes/05-self_ast_imperative)
  - [Purification](./passes/06-purification)
- [Ast\_sugar](./stages/3-ast_sugar)
  - [Self\_ast\_sugar](./passes/07-self_ast_sugar)
  - [Desugaring](./passes/08-desugaring)
- [Ast\_core](./stages/4-ast_core)
  - [Self\_ast\_core](./passes/09-self_ast_core)
  - [Inference](./passes/09.5-inference)
  - [Checking](./passes/10-checking)
- [Ast\_typed](./stages/5-ast_typed)
  - [Self\_ast\_typed](./passes/11-self_ast_typed)
  - [Spilling](./passes/12-spilling)
- [Mini C](./stages/6-mini_c)
  - [Self\_mini\_c](./passes/13-self_mini_c)
  - [Scoping](./passes/14-scoping)
- [Ligo\_coq\_ocaml.Ligo](./coq/ligo.v)
  - [Stacking](./passes/15-stacking)
    (uses the [Ligo\_coq\_ocaml.Compiler](./coq/compiler.v))
- Michelson
  - [Self\_michelson](./passes/16-self_michelson)

See also: [Predefined](./passes/predefined) (used in a few passes)
