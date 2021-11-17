type parameter =
  Increment of int
| Decrement of int

type storage = int

type return = operation list * storage

let test_param = Increment 1
let test_storage = 2

let main (action, store : parameter * storage) : return =
  let store =
    match action with
    | Increment n -> store + n
    | Decrement n -> store - n
  in ([] : operation list), store
