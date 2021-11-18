type storage = unit

let main (p, s : unit * storage) =
  (fun (f : int -> int) (x : int) -> f x)
    (fun (x : int) -> x)
    1
