open Prometheus
open Config

let blocks_produced_total =
  let help = "Total number of blocks produced" in
  Counter.v ~help ~namespace ~subsystem "blocks_produced_total"

let blocks_signed_total =
  let help = "Total number of blocks signed" in
  Counter.v ~help ~namespace ~subsystem "blocks_signed_total"

let inc_block_produced () = Prometheus.Counter.inc_one blocks_produced_total

let inc_block_signed () = Prometheus.Counter.inc_one blocks_signed_total

let operations_processed_total =
  let help = "The total number of operations processed by the node" in
  Counter.v ~help ~namespace ~subsystem "operations_processed_total"

let inc_operations_processed ~operation_count =
  Counter.inc operations_processed_total (Float.of_int operation_count)
