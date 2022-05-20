open Watcher

module type PROTOCOL_PARAMETER = sig
  val toto : string -> string
end

module Raw (P : PROTOCOL_PARAMETER) = struct
  type t

  let tick _timestamp t = (t, [])

  let process_message : Message.t -> t -> t * Message.t list =
   fun _msg _t -> failwith "Not implemented"
end

module Make (P : PROTOCOL_PARAMETER) : MAIN = Raw (P)