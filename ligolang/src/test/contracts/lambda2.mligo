type storage = unit

(* Not supported yet:
let main (a, s : unit * storage) = (fun x -> ()) ()
*)

let main (_, _ : unit * storage) =
  (fun (f : unit -> unit) -> f ()) (fun (_ : unit) -> unit)
