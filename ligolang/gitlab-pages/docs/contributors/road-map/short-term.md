---
id: short-term
title: Short term
---

## Documentation and Testing
We lack documentation and tests. Top priority.
Tests are needed at multiple levels:
Unit tests. Most utility functions should be tested individually.
Interface tests. Parts of the pipeline (Typer, Transpiler-Compiler, etc.) should be tested as black boxes.
Integration tests. Typical user scenarios, as interacted with the CLI, should be tested.
## Exposing Features
Many features have already been developed or nearly developed, and mostly need to be shown some attention, and then be exposed to the outside world, for instance:
Testing LIGO code
Step-by-step interpreter
LIGO in the browser
Propagating source locations for error messages
Dry-running a contract
## Refactoring
For the longer-term, it’s crucial to refactor big parts of the code base. This is needed to lower the complexity of the code base, so that it’s easier both for everyone (including API consumers) to interact with it.
