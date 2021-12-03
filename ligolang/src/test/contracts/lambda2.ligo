type storage is unit;

(* Not supported yet:
   let main (p:unit) storage = (fun x -> ()) ()
   *)

function main(const _unit :unit;const _storage:unit) : unit is
   block {
      const toto : (unit -> unit) -> unit = function (const f : (unit -> unit)) : unit is f (Unit); 
   } with toto(function (const _unit : unit) is Unit)
