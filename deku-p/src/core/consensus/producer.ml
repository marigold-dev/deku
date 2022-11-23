open Deku_stdlib
open Deku_concepts
open Deku_protocol
open Deku_tezos

type producer =
  | Producer of {
      operations : string Operation_hash.Map.t;
      (* TODO: should this be a set instead of map since
         we never do random access? *)
      tezos_operations : Tezos_operation.t Tezos_operation_hash.Map.t;
    }

and t = producer [@@deriving yojson]

let encoding =
  let open Data_encoding in
  conv
    (fun (Producer { operations; tezos_operations }) ->
      (operations, tezos_operations))
    (fun (operations, tezos_operations) ->
      Producer { operations; tezos_operations })
    (tup2
       (Operation_hash.Map.encoding string)
       (Tezos_operation_hash.Map.encoding Tezos_operation.encoding))

let empty =
  let operations = Operation_hash.Map.empty in
  let tezos_operations = Tezos_operation_hash.Map.empty in
  Producer { operations; tezos_operations }

let serialize_operation_to_payload operation =
  Data_encoding.Binary.to_string_exn Operation.Signed.encoding operation

(* TODO: both for produce and incoming_operations
   only add operations if they can be applied *)
let incoming_operation ~operation producer =
  let (Producer { operations; tezos_operations }) = producer in
  let operations =
    let (Operation.Signed.Signed_operation
          { initial = Initial_operation { hash; _ }; _ }) =
      operation
    in
    let operation = serialize_operation_to_payload operation in
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
        | Receipt.Vm_origination_receipt { operation = hash; _ }
        | Receipt.Vm_transaction_receipt { operation = hash; _ }
        | Receipt.Vm_transaction_error { operation = hash; _ }
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

let fill_with_noop ~identity ~level ~default_block_size operations =
  let noop =
    Operation.Signed.noop ~identity ~level ~nonce:(Nonce.of_n N.zero)
  in
  let noop = serialize_operation_to_payload noop in
  let rec fill counter operations =
    match counter <= 0 with
    | true -> operations
    | false -> fill (counter - 1) (noop :: operations)
  in
  let op_size = List.length operations in
  let dummy_op_size = Int.max (default_block_size - op_size) 0 in
  fill dummy_op_size operations

let produce ~identity ~default_block_size ~above ~withdrawal_handles_hash
    producer =
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
  let operations =
    fill_with_noop ~identity ~level ~default_block_size operations
  in
  let tezos_operations =
    List.map
      (fun (_hash, operation) -> operation)
      (Tezos_operation_hash.Map.bindings tezos_operations)
  in
  let payload = Payload.Payload operations in
  let block =
    Block.produce ~identity ~level ~previous ~payload ~withdrawal_handles_hash
      ~tezos_operations
  in
  Logs.info (fun m -> m "Producing %a" Block.pp block);
  block
