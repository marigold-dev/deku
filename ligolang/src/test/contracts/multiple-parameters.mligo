(* Test function with several parameters *)

let abcde_curried (_a : int)  (_b : int)  (c : int)  (_d : int)  (e : int) : int =
  c + e + 3

let abcde (x : int * int * int * int * int) : int =
  abcde_curried x.0 x.1 x.2 x.3 x.4
