# ppx_lambda_vm

This is a simple ppx designed to generate Lambda VM ASTs in a practical way, useful for testing and debugging.

## Usage

```ocaml
(* Lambda_vm.Ast.expr *)
let id_expr = [%lambda_vm fun x -> x]
(* same as *)
let id_expr = Lambda_vm.Ast.(Lam ("x", Var "x"))

(* Lambda_vm.Ast.value *)
let triplet_value = [%lambda_vm (1L, (2L, 3L))]
(* same as *)
let triplet_value = Lambda_vm.Ast.(Pair (Int64 1L, Pair (Int64 2L, Int64 3L)))

(* Lambda_vm.Ast.script *)
let add_script = [%lambda_vm.script fun pair -> fst pair + snd pair ]
(* same as *)
let add_script =
  Lambda_vm.Ast.
    {
      param = "pair";
      code =
        App
          {
            funct =
              App
                {
                  funct = Prim Add;
                  arg = App { funct = Prim Fst; arg = Var "pair" };
                };
            arg = App { funct = Prim Snd; arg = Var "snd" };
          };
    }
```

## Escape

Sometimes you may want to compose different expressions from fragments, both `[%lambda_vm]` and `[%lambda_vm.script]` support expressions escapes through `[%e expr]`

Example:

```ocaml
let make_add a b = [%lambda_vm [%e a] + [%e b]]
(* same as *)
let make_add a b =
      Lambda_vm.Ast.(App { funct = App { funct = Prim Add; arg = a }; arg = b })
```

## Primitives

All the primitives identifiers are reserved, they're: "not", "+", "-", "\*", "/", "mod", "land", "lor", "lxor", "lsl", "lsr", "asr", "fst", "snd".

They're replaced when the identifier is present, so

```ocaml
let make_not a = [%lambda_vm not [%e a]]
(* becomes *)
let make_not a = Lambda_vm.Ast.(App { funct = Prim Neg; arg = a })

let prim_not = [%lambda_vm not]
(* becomes *)
let prim_not = Lambda_vm.Ast.(Prim Neg)

let prim_add = [%lambda_vm ( + )]
let prim_add = Lambda_vm.Ast.(Prim Add)
```
