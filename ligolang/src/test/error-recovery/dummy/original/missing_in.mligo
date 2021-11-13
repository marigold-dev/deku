type storage = int

type parameter = int list

type return = operation list * storage

let hd (x : int list) : int =
  let err = -1 in
  match x with
  | [] -> err
  | x :: xs -> x

let main (a, b : parameter * storage) : return =
  ([] : operation list), (hd a + (b + 8) * 11)
