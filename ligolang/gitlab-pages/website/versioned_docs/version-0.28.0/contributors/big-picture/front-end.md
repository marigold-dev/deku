---
id: front-end
title: Front End
---

The Front-End is the part that deals with all syntactical matters. LIGO supports multiple Front-Ends. Each Front-End is composed of three different parts.
## Parser
A Parser is a function that takes a string of source code and outputs a structured representation of the program. This part usually uses Menhir, a parser generator compatible with OCaml.
Its files are in `parser/parser_name`.
## Concrete Syntax Tree
The CST is the aforementioned structured representation of the program. Is is structurally very close to the source code, and is mostly an intermediary there because manipulating string is not practical.
Its files are in `parser/parser_name`.
## Sugar_to_core
A Sugar_to_core is a function that takes a CST and outputs the corresponding Common AST. This is the actual bridge between a given syntax and LIGO.
Its files are in `simplify/parser_name`.
