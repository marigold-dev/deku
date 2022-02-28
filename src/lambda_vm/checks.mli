exception Out_of_stack
exception Out_of_gas

val max_stack_depth : int

val check_stack : stack:int -> unit
val check_gas : Gas.t -> unit
