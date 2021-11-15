let main (p, s : nat list * (unit -> unit)) : operation list * (unit -> unit) =
  match p with
  | [] -> (([] : operation list), s)
  | x :: xs ->
    let f = fun (_ : unit) -> failwith (x + x + List.length xs) in
    (([] : operation list), f)
