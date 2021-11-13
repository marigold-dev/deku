
type parameter =
  Increment of (int * int * int)
| Reset

let main (action : parameter) : int =
  match action with
  | Increment (n, m) -> 0
  | Reset            -> 0
