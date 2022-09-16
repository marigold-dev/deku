open Deku_stdlib

(* TODO: this being in blocks is really weird *)
let includable_operation_window = Option.get (N.of_z (Z.of_int 120))
let block_timeout = 10.0

let clean_block_pool_time =
  let minutes = 60.0 in
  (* let minutes = 0.1 in *)
  60.0 (* seconds *) *. minutes

let clean_gossip_time =
  let minutes = 60.0 *. 2.0 (* hours *) in
  (* let minutes = 0.2 in *)
  60.0 (* seconds *) *. minutes
