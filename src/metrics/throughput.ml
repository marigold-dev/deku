open Prometheus
open Config

let operations_per_block =
  let help = "The number of operations in each block" in
  Gauge.v ~help ~namespace ~subsystem "operations_per_block"

let block_rate =
  let help = "Time between blocks" in
  Gauge.v ~help ~namespace ~subsystem "block_rate"

let previous_timestamp = ref None

let collect_block_metrics ~timestamp ~operation_count =
  Gauge.set operations_per_block (Float.of_int operation_count);
  (match !previous_timestamp with
  | Some previous_timestamp ->
    let delta = timestamp -. previous_timestamp in
    Gauge.set block_rate delta
  | None -> ());
  previous_timestamp := Some timestamp
