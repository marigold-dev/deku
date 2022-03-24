exception Out_of_stack
type error = [`Out_of_stack] [@@deriving show]

val max_stack_depth : int

val check_stack : stack:int -> unit
