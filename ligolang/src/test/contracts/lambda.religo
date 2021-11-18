type storage = unit;

/* not supported yet
   let%entry main (p:unit) storage =
     (fun x -> ()) ()
   */

let main = ((_,_) : (unit, storage)) : unit =>
  (((_useless : unit)) => ()) ();
