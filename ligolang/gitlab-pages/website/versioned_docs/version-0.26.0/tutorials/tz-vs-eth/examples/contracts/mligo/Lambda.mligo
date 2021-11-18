type parameter = Compute of int -> int | Set of int

type storage = int

let main (p, s : parameter * storage) =
  match p with
    Compute func -> ([] : operation list), func s
  | Set n -> ([] : operation list), n
