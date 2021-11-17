(*_*
  name: Increment (CameLIGO)
  language: cameligo
  compile:
    entrypoint: main
  dryRun:
    entrypoint: main
    parameters: Increment 1
    storage: 0
  deploy:
    entrypoint: main
    storage: 0
  evaluateValue:
    entrypoint: ""
  evaluateFunction:
    entrypoint: add
    parameters: 5, 6
  generateDeployScript:
    tool: tezos-client
    entrypoint: main
    storage: 0
*_*)
type storage = int

type parameter =
  Increment of int
| Decrement of int
| Reset

type return = operation list * storage

// Two entrypoints

let add (store, delta : storage * int) : storage = store + delta
let sub (store, delta : storage * int) : storage = store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
   
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
