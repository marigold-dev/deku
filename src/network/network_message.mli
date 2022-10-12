type message = private
  | Message of { raw_header : string; raw_content : string }
  | Request of { raw_header : string; raw_content : string }

type t = message

(* constructors *)
val message : raw_header:string -> raw_content:string -> message
val request : raw_header:string -> raw_content:string -> message

(* repr *)

val encoding : message Data_encoding.t
