// variant defining entrypoints

type action is
  Increment of int
| Decrement of int

type return is list (operation) * int

function add (const a : int; const b : int) : int is a + b

function subtract (const a : int; const b : int) : int is a - b

// main function routing the flow based on the action provided

function main (const p : action; const s : int) : return is
  ((nil : list (operation)),
    case p of
      Increment (n) -> add (s, n)
    | Decrement (n) -> subtract (s, n)
    end)
