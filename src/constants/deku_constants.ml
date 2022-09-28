open Deku_stdlib

(* TODO: this being in blocks is really weird *)
let includable_operation_window = Option.get (N.of_z (Z.of_int 120))
let listen_timeout = 1.0
let reconnect_timeout = 5.0
let block_timeout = 10.0
let _genesis_time = 0.

let clean_block_pool_time =
  let minutes = 60.0 in
  (* let minutes = 0.1 in *)
  60.0 (* seconds *) *. minutes

let clean_gossip_time =
  let minutes = 60.0 *. 2.0 (* hours *) in
  (* let minutes = 0.2 in *)
  60.0 (* seconds *) *. minutes

let async_on_error exn = Format.eprintf "async: %s\n%!" (Printexc.to_string exn)
let genesis_time = 0.0
