exception Invalid_value

type t

(* TODO: decide on whether we accept crypto primitives or michelson*)
val make : sender:string -> source:string -> Gas.t -> t

val source : t -> unit -> Ir.value

val sender : t -> unit -> Ir.value

val gas : t -> Gas.t
