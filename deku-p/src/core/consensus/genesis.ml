open Deku_stdlib
open Deku_concepts
open Deku_crypto

let identity =
  let b58 = "edsk2icTz8hTkajZKfqcH3V5aJnBd5AHavJLGmzi8mQo16zW4Z36zS" in
  match Secret.of_b58 b58 with
  | Some secret -> Identity.make secret
  | None -> failwith "bug when parsing edsk key"

let block =
  Eio_main.run @@ fun env ->
  Parallel.Pool.run ~env ~domains:1 @@ fun () ->
  let level = Level.zero in
  let withdrawal_handles_hash = BLAKE2b.hash "tuturu" in
  let previous =
    Block_hash.hash ~block_level:level
      ~block_payload_hash:(BLAKE2b.hash "mayushi")
      ~state_root_hash:(BLAKE2b.hash "desu") ~withdrawal_handles_hash
  in
  let payload = "" in
  Block.produce ~identity ~level ~previous ~payload
