open Watcher

module type BLOCK_POOL_PARAMETER = sig
  module Threshold : sig
    val get : Helpers.Height.t -> int
  end
end

module Raw (P : BLOCK_POOL_PARAMETER) = struct
  module Action = struct
    open Bin_prot.Std
    type t =
      | Block_by_hash of bytes
      | Block_level   of int64
    [@@deriving ord, bin_io]
  end

  type t = {
    temp : bool;
        (* block_pool : Node.Block_pool.t;
           block_and_signatures : Block_pool.block_and_signatures; *)
  }

  let tick _timestamp t = (t, [])

  let process_message : Message.t -> t -> t * Message.t list =
   fun msg _t ->
    (* TODO: verify sender, sig, etc *)
    let payload_bytes : bytes = Message.get_payload msg in
    let payload : Action.t =
      Pollinate.Util.Encoding.unpack Action.bin_read_t payload_bytes in
    match payload with
    | _ -> failwith "Not implemented"

  let filter_msgs _msgs = failwith "Not implemented"
end

module Make (P : BLOCK_POOL_PARAMETER) : MAIN = Raw (P)
