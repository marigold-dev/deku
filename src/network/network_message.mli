type message = private
  | Message of { raw_expected_hash : string; raw_content : string }
  | Request of { raw_expected_hash : string; raw_content : string }

type t = message

val max_size : int

(* constructors *)
val message : raw_expected_hash:string -> raw_content:string -> message
val request : raw_expected_hash:string -> raw_content:string -> message

(* communication *)
exception Invalid_tag
exception Invalid_message_size

val read : Eio.Buf_read.t -> message
val write : Eio.Buf_write.t -> message -> unit
