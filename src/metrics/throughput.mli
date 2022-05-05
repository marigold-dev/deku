(* Define the metrics of TPS aka throughput of Deku chain

   [collect_block_metrics timestamp operation_count]
   collect the information of block base on the timestamp and the number of operations
*)

val collect_block_metrics : timestamp:float -> operation_count:int -> unit
