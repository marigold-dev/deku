open Deku_concepts
open Deku_protocol
open State

type producer =
  | Producer of {
      identity : Identity.t;
      operations : Operation.t Operation_hash.Map.t;
    }

and t = producer

let make ~identity =
  let operations = Operation_hash.Map.empty in
  Producer { identity; operations }

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { identity; operations }) = producer in
  let operations =
    let (Operation.Operation { hash; _ }) = operation in
    Operation_hash.Map.add hash operation operations
  in
  Producer { identity; operations }

let clean ~receipts producer =
  let (Producer { identity; operations }) = producer in
  let operations =
    List.fold_left
      (fun operations receipt ->
        let (Receipt.Receipt { operation = hash }) = receipt in
        Operation_hash.Map.remove hash operations)
      operations receipts
  in
  Producer { identity; operations }

let produce ~current_level ~current_block producer =
  let (Producer { identity; operations }) = producer in
  let previous = current_block in
  let level = Level.next current_level in
  let operations =
    List.map
      (fun (_hash, operation) -> operation)
      (Operation_hash.Map.bindings operations)
  in
  Block.produce ~identity ~level ~previous ~operations

let produce ~current ~state producer =
  let (State { current_level; current_block; _ }) = state in
  let (Producer { identity; operations = _ }) = producer in

  match
    let self = Identity.key_hash identity in
    Judger.is_producer ~current ~state self
  with
  | true ->
      let block = produce ~current_level ~current_block producer in
      Format.printf "Producing %a\n%!" Block.pp block;
      Some block
  | false -> None
