# Hacking

## Pipeline operator

In function programming it's common to use static dispatching by default, that means `v.f()` is not available, instead we do `f(v)`, but this doesn't scale well, as two operations instead of being `v.f().g()` is now `g(f(v))`, to workaround that we have the pipeline operator, which is just an apply function with the signature `'a -> ('a -> 'b) -> b`, so doing `v |> f |> g` is identical to doing `g(f(v))`.

Sometimes you will see it being used even for single cases `v |> f`, this is more of a stylistic and consistency choice.

## OCaml vocabulary

### t

A type named t means the main type of a module so, `Validators.t` can be read as `validators` and `Wallet.t` as `wallet`.

An identifier named t on a module will always have type t, so under `Validators.re` `let t = empty` is the same as `let validators = empty`

## OCaml Style Guide

### OCaml Basics
- Never use the `ignore` function. Instead prefer `let (_: typ) = ...`
- Never use the `@@` operator.
- Never add extraneous type annotations.
- Never use infix operators to chain monads together. Instead use `let` syntax.

### LWT
- Never ignore the result of an `Lwt` promise. Instead, if a promise is
  executed for side-effects only, use `Lwt.async` or just return the `Lwt.t`.