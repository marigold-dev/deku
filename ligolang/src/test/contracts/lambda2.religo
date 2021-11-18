type storage = unit;

/* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   */

let main = ((_, _) : (unit, storage)) : unit =>
  ((f : (unit => unit)) => f ()) ((_useless : unit) => unit);
