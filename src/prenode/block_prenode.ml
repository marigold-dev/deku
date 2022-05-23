open Watcher

module type BLOCK_PARAMETER = sig
  val toto : string -> string
end

module Raw (P : BLOCK_PARAMETER) = struct
  type t

  let tick _timestamp t = (t, [])

  let process_message : Message.t -> t -> t * Message.t list =
   fun _msg _t -> failwith "Not implemenetd"

  let filter_msgs _msgs = failwith "Not implemented"
end

module Make (P : BLOCK_PARAMETER) : MAIN = Raw (P)
