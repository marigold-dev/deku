open Deku_stdlib

(* TODO: this being in blocks is really weird *)
let includable_operation_window = Option.get (N.of_z (Z.of_int 120))
let listen_timeout = 1.0
let reconnect_timeout = 5.0
let block_timeout = 10.0

let clean_block_pool_time =
  let minutes = 60.0 in
  (* let minutes = 0.1 in *)
  60.0 (* seconds *) *. minutes

let clean_gossip_time =
  let minutes = 60.0 *. 2.0 (* hours *) in
  (* let minutes = 0.2 in *)
  60.0 (* seconds *) *. minutes

let async_on_error exn =
  Logs.err (fun m -> m "async: %s" (Printexc.to_string exn))

let genesis_time = 0.0
let trusted_cycle = Option.get (N.of_z (Z.of_int 600))
let max_payload_chunks = 32
let request_timeout = 30.0
