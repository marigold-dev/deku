
type parameter = { one : int ; two : int }

let main (action : parameter) : int =
  match action with
  | {one = _ ; three = _} -> 0
