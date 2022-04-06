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
