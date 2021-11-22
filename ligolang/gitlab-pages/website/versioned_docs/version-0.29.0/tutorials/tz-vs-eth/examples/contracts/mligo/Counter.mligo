type parameter = Add of int | Subtract of int

type storage = int

let main (p, s : parameter * storage) =
  match p with
    Add n -> ([] : operation list), s + n
  | Subtract n -> ([] : operation list), s - n
