module Stateful () : sig
  val log : string -> unit
  val get : unit -> string
end = struct

  let logger = ref ""
  let log : string -> unit =
    fun s -> logger := !logger ^ s
  let get () : string = !logger

end
