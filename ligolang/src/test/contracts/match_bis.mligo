type storage = int

type parameter =
  Increment of int
| Decrement of int

type return = operation list * storage

let add (a : int) (b : int) : int = a + b
let sub (a : int) (b : int) : int = a - b

let main (action, store : parameter * storage) : return =
  let store =
    match action  with
      Increment n -> add store n
    | Decrement n -> sub store n
  in ([] : operation list), store
