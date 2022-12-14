open Deku_concepts
open Deku_protocol
open Deku_consensus

type fragment
type outcome
type producer
type t = producer

val empty : producer

val incoming_raw_operation :
  current_level:Level.t ->
  Protocol_operation.Raw.t ->
  producer ->
  fragment option

(* fragments *)
val compute : fragment -> outcome
(** [compute fragment] Can be executed in parallel *)

val apply : current_level:Level.t -> outcome:outcome -> producer -> producer

(* internal *)
val tag_included :
  current_level:Level.t -> Protocol_operation.Raw.t -> producer -> producer

val produce :
  identity:Identity.t -> above:Block.t -> limit:int -> producer -> Block.t

val drop : current_level:Level.t -> producer -> producer

(* repr *)
val encoding : producer Data_encoding.t
