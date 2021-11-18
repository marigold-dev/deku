---
id: middle-end
title: Middle End
---

The Middle-End is the core of LIGO. It is also composed of three parts.
## Common AST
The Common AST is the closest thing to what could be called “LIGO lang”. As such, it should be as simple as possible. Collapsing particular cases in more general constructs is encouraged. Documenting it is crucial for people who’ll write new parsers or editor support for Front-end related things.
Its files are in `ast_core/`, of interest is the definition of the AST itself in `ast_core/types.ml`.
## Type Checker
The Type Checker, among other things, checks that a given AST is valid with regard to type-safety. It also annotates expressions with their types, free-variables and local environments.
As time passes, we want to make the type-system stronger, to encode arbitrarily complex properties in an extensible manner.
Its files are in `typer/`.
## Typed AST
The Typed AST is the result of Type Checker. On top of it, we also want to define/export many features aiming at making building tools on top LIGO as easy as possible.
Its files are in `ast_typed/`, of interest is the definition of the Typed AST itself in `ast_typed/types.ml`.
