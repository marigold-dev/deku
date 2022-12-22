open Deku_stdlib

val includable_operation_window : N.t
val listen_timeout : float
val reconnect_timeout : float
val block_timeout : float
val clean_block_pool_time : float
val clean_gossip_time : float
val async_on_error : exn -> unit
val genesis_time : float
val trusted_cycle : N.t
val max_payload_chunks : int
val request_timeout : float
