val inc_block_produced : unit -> unit

val inc_block_signed : unit -> unit

val inc_operations_processed : operation_count:int -> unit

val block_metrics : timestamp:float -> operation_count:int -> unit
