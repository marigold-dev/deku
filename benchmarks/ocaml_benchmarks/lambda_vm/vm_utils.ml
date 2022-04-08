open Lambda_vm

let failwith s = Format.kasprintf failwith s

(* compile value *)
let compile_value_exn gas value =
  match compile_value gas value with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

(* compile script *)
let compile_exn gas script =
  match compile gas script with
  | Ok value -> value
  | Error error -> failwith "Compilation_error(%a)" pp_compile_error error

(* execute script *)
let execute_exn gas arg script =
  match execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith "Execution_error(%a)" pp_execution_error error
