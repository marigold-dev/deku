type message = private
  | Message of { raw_header : string; raw_fragments : string list }
  | Request of { raw_header : string; raw_fragments : string list }

type t = message

val max_fragment_size : int

(* constructors *)
val message : raw_header:string -> raw_fragments:string list -> message
val request : raw_header:string -> raw_fragments:string list -> message

(* communication *)
exception Invalid_message

val read : Eio.Buf_read.t -> message
val write : Eio.Buf_write.t -> message -> unit
