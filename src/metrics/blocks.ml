open Prometheus
open Deku_concepts
open Deku_stdlib

let blocks_produced =
  let help = "Number of blocks produced" in
  Counter.v ~help "deku_blocks_produced"

let current_level =
  let help = "Current block level" in
  Gauge.v ~help "deku_current_level"

let save_block Deku_consensus.Block.(Block block) =
  Counter.inc_one blocks_produced;
  let level = Level.to_n block.level |> N.to_z |> Z.to_float in
  Gauge.set current_level level

let transactions_per_block =
  let help = "Number of transactions in a block, based on block size" in
  Gauge.v ~help "deku_transactions_per_block"

let set_transactions_per_block block_size =
  Gauge.set transactions_per_block @@ float_of_int block_size
