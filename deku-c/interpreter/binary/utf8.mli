type codepoint = Uchar.t
type t [@@deriving equal, compare]

exception Utf8

val decode : string -> t (* raises Utf8 *)
val encode : t -> string (* raises Utf8 *)
