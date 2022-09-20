open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_tezos
open State

type producer =
  | Producer of {
      identity : Identity.t;
      operations : Operation.t Operation_hash.Map.t;
      (* TODO: should this be a set instead of map since
         we never do random access? *)
      tezos_operations : Tezos_operation.t Tezos_operation_hash.Map.t;
      default_block_size : int;
    }

and t = producer

let make ~identity ~default_block_size =
  let operations = Operation_hash.Map.empty in
  let tezos_operations = Tezos_operation_hash.Map.empty in
  Producer { identity; operations; tezos_operations; default_block_size }

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { identity; operations; tezos_operations; default_block_size })
      =
    producer
  in
  let operations =
    let (Operation.Operation { hash; _ }) = operation in
    Operation_hash.Map.add hash operation operations
  in
  Producer { identity; operations; tezos_operations; default_block_size }

let incoming_tezos_operation ~tezos_operation producer =
  let (Producer { identity; operations; tezos_operations; default_block_size })
      =
    producer
  in
  let tezos_operations =
    let Tezos_operation.{ hash; _ } = tezos_operation in
    Tezos_operation_hash.Map.add hash tezos_operation tezos_operations
  in
  Producer { identity; operations; tezos_operations; default_block_size }

let clean ~receipts ~tezos_operations producer =
  let (Producer
        {
          identity;
          operations;
          tezos_operations = old_tezos_operations;
          default_block_size;
        }) =
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
  Producer { identity; operations; tezos_operations; default_block_size }

let produce ~parallel_map ~current_level ~current_block producer =
  let (Producer { identity; operations; tezos_operations; default_block_size })
      =
    producer
  in
  let previous = current_block in
  let level = Level.next current_level in
  let operations =
    List.map
      (fun (_hash, operation) -> operation)
      (Operation_hash.Map.bindings operations)
  in
  let op_size = List.length operations in
  let dummy_op_size = Int.max (default_block_size - op_size) 0 in
  let noop = Operation.noop ~identity ~level ~nonce:(Nonce.of_n N.zero) in
  let dummy_operations = List.init dummy_op_size (fun _ -> noop) in
  let operations = List.rev_append dummy_operations operations in
  let tezos_operations =
    List.map
      (fun (_hash, operation) -> operation)
      (Tezos_operation_hash.Map.bindings tezos_operations)
  in
  Block.produce ~parallel_map ~identity ~level ~previous ~operations
    ~tezos_operations

let produce ~parallel_map ~current ~state producer =
  let (State { current_level; current_block; _ }) = state in
  let (Producer
        {
          identity;
          operations = _;
          tezos_operations = _;
          default_block_size = _;
        }) =
    producer
  in

  match
    let self = Identity.key_hash identity in
    Judger.is_producer ~current ~state self
  with
  | true ->
      let block =
        produce ~parallel_map ~current_level ~current_block producer
      in
      Logs.info (fun m -> m "Producing %a" Block.pp block);
      Some block
  | false -> None
