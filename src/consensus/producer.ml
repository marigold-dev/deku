open Deku_concepts
open Deku_protocol

type producer = Producer of { operations : Operation.t Operation_hash.Map.t }
and t = producer [@@deriving yojson]

let empty =
  let operations = Operation_hash.Map.empty in
  Producer { operations }

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { operations }) = producer in
  let operations =
    let (Operation.Operation { hash; _ }) = operation in
    Operation_hash.Map.add hash operation operations
  in
  Producer { operations }

let clean ~receipts producer =
  let (Producer { operations }) = producer in
  let operations =
    List.fold_left
      (fun operations receipt ->
        let (Receipt.Receipt { operation = hash }) = receipt in
        Operation_hash.Map.remove hash operations)
      operations receipts
  in
  Producer { operations }

let produce ~identity ~above producer =
  let open Block in
  let (Block { hash = current_block; level = current_level; _ }) = above in
  let (Producer { operations }) = producer in
  let previous = current_block in
  let level = Level.next current_level in
  let operations =
    List.map
      (fun (_hash, operation) -> operation)
      (Operation_hash.Map.bindings operations)
  in
  Block.produce ~identity ~level ~previous ~operations
