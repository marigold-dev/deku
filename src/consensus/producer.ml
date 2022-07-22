open Deku_concepts
open Deku_protocol

type producer =
  | Producer of { identity : Identity.t; operations : Operation.Set.t }

and t = producer

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { identity = _; operations }) = producer in
  
  ()

let produce ~current_level ~previous producer = 
  let (Producer { identity; operations }) = producer in
  let level = Level.next current_level in
  let payload = Operation
  let x = Block.produce ~identity ~level ~previous ~payloadin
  assert false
