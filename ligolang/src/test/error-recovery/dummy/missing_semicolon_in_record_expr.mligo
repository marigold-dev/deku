type storage = int

type parameter = int list

type s = {
  a : int;
  b : bool;
  c : string;
  d : int list list list
}

let s_a = { a = 42 b = false }

let s_b = {s with a = 32}

type return = operation list * storage

let hd (x : int list) : int =
  match x with
  | [] -> -1
  | x :: xs -> x

let main (a, b : parameter * storage) : return =
  ([] : operation list), (hd a + (b + 8) * 11)
