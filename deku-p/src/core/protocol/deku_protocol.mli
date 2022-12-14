module Protocol_operation = Protocol_operation
module Protocol_receipt = Protocol_receipt
module Protocol_payload = Protocol_payload
open Deku_concepts

type protocol
type t = protocol

type seq_map_p = { f : 'a 'b. ('a -> 'b) -> 'a Seq.t -> 'b Seq.t }
[@@ocaml.unboxed]

val initial : protocol

val apply :
  seq_map_p:seq_map_p ->
  current_level:Level.t ->
  payload:Protocol_payload.t ->
  protocol ->
  protocol * Protocol_receipt.t

(* repr *)
val encoding : protocol Data_encoding.t
