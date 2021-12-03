(*_*
  name: Increment (JsLIGO)
  language: jsligo
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
type storage = int;

type parameter =
| ["Increment", int]
| ["Decrement", int]
| ["Reset"];

type return_ = [list <operation>, storage];

/* Two entrypoints */
let add = ([store, delta] : [storage, int]) : storage => store + delta;
let sub = ([store, delta] : [storage, int]) : storage => store - delta;

/* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. */
let main = ([action, store] : [parameter, storage]) : return_ => {
 return [
   (list([]) as list <operation>),    // No operations
   (match (action, {
    Increment: (n: int) => add ([store, n]),
    Decrement: (n: int) => sub ([store, n]),
    Reset:     ()  => 0}))
  ]
};

