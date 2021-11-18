type action is
| Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt of int
// | Increment of int
| Decrement of int

function add (const a : int ; const b : int) : int is a + b

function subtract (const a : int ; const b : int) : int is a - b

function main (const p : action ; const s : int) : (list(operation) * int) is 
  ((nil : list(operation)),
    case p of
    | Incrementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt (n) -> add (s, n)
    // | Increment(n) -> add (s, n)
    | Decrement (n) -> subtract (s, n)
    end)

// incrementttttttttttttttttttttttt