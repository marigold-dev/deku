open Prometheus
open Config

(***************************************************************************)
(* Metrics of the number of blocks produced in total *)

let blocks_produced_total =
  let help = "Total number of blocks produced" in
  Counter.v ~help ~namespace ~subsystem "blocks_produced_total"

let inc_block_produced () = Prometheus.Counter.inc_one blocks_produced_total

(***************************************************************************)
(* Metrics of the number of blocks signed in total *)
let blocks_signed_total =
  let help = "Total number of blocks signed" in
  Counter.v ~help ~namespace ~subsystem "blocks_signed_total"

let inc_block_signed () = Prometheus.Counter.inc_one blocks_signed_total

(***************************************************************************)
(* Metrics of the numbers of added operations per block in total *)
let operations_per_block =
  let help = "The number of operations in each block" in
  Counter.v ~help ~namespace ~subsystem "operations_per_block"

let inc_operations_processed ~operation_count =
  Counter.inc operations_per_block (Float.of_int operation_count)

(***************************************************************************)
(* Metrics block metrics:
   The rate of block produces
*)

let previous_timestamp = ref None

let block_rate =
  let help = "Time between blocks" in
  Gauge.v ~help ~namespace ~subsystem "block_rate"

let block_metrics ~timestamp ~operation_count =
  inc_operations_processed ~operation_count;
  match !previous_timestamp with
  | None ->
    ();
    previous_timestamp := Some timestamp
  | Some previous_timestamp ->
    let delta = timestamp -. previous_timestamp in
    Gauge.set block_rate delta
