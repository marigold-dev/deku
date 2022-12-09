open Deku_concepts

type message_pool
type t = message_pool

val encoding : message_pool Data_encoding.t

(* compute *)
type fragment
type outcome

type action = private
  | Message_pool_message of { message : Message.t }
  | Message_pool_fragment of { fragment : fragment }

val initial : message_pool
val encode : content:Message.Content.t -> fragment

val decode :
  raw_header:string ->
  raw_content:string ->
  message_pool ->
  message_pool * fragment option

val apply : outcome:outcome -> message_pool -> message_pool * action option
val compute : fragment -> outcome
val close : until:Level.t -> message_pool -> message_pool
