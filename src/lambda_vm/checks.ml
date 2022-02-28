exception Out_of_stack
exception Out_of_gas

(* TODO: reasonable number here *)
let max_stack_depth = 1000

let check_stack ~stack = if stack <= 0 then raise Out_of_stack
let check_gas gas = if Gas.is_empty gas then raise Out_of_gas
