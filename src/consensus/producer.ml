open Deku_concepts
open Deku_protocol

type producer =
  | Producer of { identity : Identity.t; operations : Operation.Set.t }

and t = producer

let make ~identity =
  let operations = Operation.Set.empty in
  Producer { identity; operations }

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { identity; operations }) = producer in
  let operations = Operation.Set.add operation operations in
  Producer { identity; operations }

let produce ~current_level ~previous producer =
  let (Producer { identity; operations }) = producer in
  let level = Level.next current_level in
  let operations = Operation.Set.elements operations in
  Block.produce ~identity ~level ~previous ~operations
