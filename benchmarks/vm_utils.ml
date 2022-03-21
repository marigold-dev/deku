open Lambda_vm

let failwith s = Format.kasprintf failwith s

(* compile value *)
let compile_value_exn gas value =
  match compile_value gas value with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

let compile_value_n n ~initial_gas =
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

(* compile script *)
let compile_exn gas script =
  match compile gas script with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

let compile_script ~initial_gas script =
  let gas = Gas.make ~initial_gas in
  compile_exn gas script

(* execute script *)
let execute_exn gas arg script =
  match execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" pp_execution_error error
