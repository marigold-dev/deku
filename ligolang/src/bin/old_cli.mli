open Cmdliner

val buffer : Buffer.t
val run : ?argv:string array -> unit -> unit Term.result
