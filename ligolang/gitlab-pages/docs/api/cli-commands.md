---
id: cli-commands
title: CLI Commands
---

Contracts written in LIGO can be compiled using the `ligo` executable.


## Compiling a contract

Compile your contract with a specific entry point.

```zsh
ligo compile contract SOURCE_FILE --entry-point ENTRY_POINT
```

#### Example

```zsh
ligo compile contract examples/counter.ligo --entry-point main
```

## Defining the initial storage

If your contract implements a sophisticated storage, you can compile a LIGO expression into a Michelson value quite easily.

```zsh
ligo compile storage SOURCE_FILE EXPRESSION --entry-point ENTRY_POINT
```

#### Example
```zsh
ligo compile storage examples/counter.ligo 5 --entry-point main
# Outputs: 5
```

## Invoking the contract with a parameter

```zsh
ligo compile parameter SOURCE_FILE EXPRESSION --entry-point ENTRY_POINT
```

#### Example
```zsh
ligo compile parameter examples/counter.ligo "Increment(5)" --entry-point main
# Outputs: (Right 5)
```