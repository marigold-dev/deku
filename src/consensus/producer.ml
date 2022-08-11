open Deku_concepts
open Deku_protocol
open Deku_tezos
open Consensus

type producer =
  | Producer of {
      identity : Identity.t;
      operations : Operation.t Operation_hash.Map.t;
      tezos_operations : Tezos_operation.t Tezos_operation_hash.Map.t;
    }

and t = producer

let make ~identity =
  let operations = Operation_hash.Map.empty in
  let tezos_operations = Tezos_operation_hash.Map.empty in
  Producer { identity; operations; tezos_operations }

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { identity; operations; tezos_operations }) = producer in
  let operations =
    let (Operation.Operation { hash; _ }) = operation in
    Operation_hash.Map.add hash operation operations
  in
  Producer { identity; operations; tezos_operations }

let incoming_tezos_operation ~tezos_operation producer =
  let (Producer { identity; operations; tezos_operations }) = producer in
  let tezos_operations =
    let Tezos_operation.{ hash; _ } = tezos_operation in
    Tezos_operation_hash.Map.add hash tezos_operation tezos_operations
  in
  Producer { identity; operations; tezos_operations }

let clean ~receipts ~tezos_operations producer =
  let (Producer
        { identity; operations; tezos_operations = old_tezos_operations }) =
    producer
  in
  let operations =
    List.fold_left
      (fun operations receipt ->
        let (Receipt.Receipt { operation = hash }) = receipt in
        Operation_hash.Map.remove hash operations)
      operations receipts
  in
  let tezos_operations =
    List.fold_left
      (fun tezos_operations tezos_operation ->
        Tezos_operation_hash.Map.remove tezos_operation.Tezos_operation.hash
          tezos_operations)
      old_tezos_operations tezos_operations
  in
  Producer { identity; operations; tezos_operations }

let produce ~parallel_map ~current_level ~current_block producer =
  (* TODO: Make this configurable *)
  let max_block_size = 50_000 in
  let (Producer { identity; operations; tezos_operations }) = producer in
  let previous = current_block in
  let level = Level.next current_level in
  let operations =
    List.map
      (fun (_hash, operation) -> operation)
      (Operation_hash.Map.bindings operations)
  in
  let op_size = List.length operations in
  let dummy_op_size =  Int.max (max_block_size - op_size) 0 in
  let dummy_operations =
    List.init dummy_op_size (fun _ -> Operation.noop ~level)
  in
  let operations = List.rev_append dummy_operations operations in
  let tezos_operations =
    List.map
      (fun (_hash, operation) -> operation)
      (Tezos_operation_hash.Map.bindings tezos_operations)
  in
  Block.produce ~parallel_map ~identity ~level ~previous ~operations
    ~tezos_operations

let try_to_produce ~parallel_map ~current ~consensus producer =
  let (Consensus { current_level; current_block; _ }) = consensus in
  let (Producer { identity; operations = _; tezos_operations = _ }) =
    producer
  in
  match
    let self = Identity.key_hash identity in
    is_expected_author ~current ~author:self consensus
  with
  | true ->
      let block =
        produce ~parallel_map ~current_level ~current_block producer
      in
      Format.printf "Producing %a \n%!" Block.pp block;
      Some block
  | false -> None
