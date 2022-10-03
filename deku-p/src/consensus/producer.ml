open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_tezos

type producer =
  | Producer of {
      operations : Operation.t Operation_hash.Map.t;
      (* TODO: should this be a set instead of map since
         we never do random access? *)
      tezos_operations : Tezos_operation.t Tezos_operation_hash.Map.t;
    }

and t = producer [@@deriving yojson]

let empty =
  let operations = Operation_hash.Map.empty in
  let tezos_operations = Tezos_operation_hash.Map.empty in
  Producer { operations; tezos_operations }

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { operations; tezos_operations }) = producer in
  let operations =
    let (Operation.Operation { hash; _ }) = operation in
    Operation_hash.Map.add hash operation operations
  in
  Producer { operations; tezos_operations }

let incoming_tezos_operation ~tezos_operation producer =
  let (Producer { operations; tezos_operations }) = producer in
  let tezos_operations =
    let Tezos_operation.{ hash; _ } = tezos_operation in
    Tezos_operation_hash.Map.add hash tezos_operation tezos_operations
  in
  Producer { operations; tezos_operations }

let clean ~receipts ~tezos_operations producer =
  let (Producer { operations; tezos_operations = old_tezos_operations }) =
    producer
  in
  let operations =
    List.fold_left
      (fun operations receipt ->
        match receipt with
        | Receipt.Ticket_transfer_receipt { operation = hash }
        | Receipt.Vm_transaction_receipt { operation = hash }
        | Receipt.Withdraw_receipt { operation = hash; _ } ->
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
  Producer { operations; tezos_operations }

let produce ~identity ~default_block_size ~parallel_map ~above
    ~withdrawal_handles_hash producer =
  let open Block in
  let (Producer { operations; tezos_operations }) = producer in
  let (Block { hash = current_block; level = current_level; _ }) = above in
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
  let block =
    Block.produce ~parallel_map ~identity ~level ~previous ~operations
      ~withdrawal_handles_hash ~tezos_operations
  in
  Logs.info (fun m -> m "Producing %a" Block.pp block);
  block
