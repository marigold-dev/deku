exception Parse_error

let parse code =
  match Wasm_vm.Module.of_string ~gas:(ref max_int) ~code with
  | Ok module_ -> module_
  | Error _ -> raise Parse_error

let test_successful_parsing () =
  let code =
    {|
    (module
      (import "env" "syscall" (func $syscall (param i64) (result i32)))
      (memory $mem 1)
      (export "memory" (memory $mem))
      (type $sum_t (func (param i32 i32) (result i32)))
      (type $main_t (func (param i32) (result i64 i64)))
      (func $sum_f (type $sum_t) (param $x i32) (param $y i32) (result i32)
        i32.const 0
        local.get $x
        local.get $y
        i32.add
        i32.store
        i32.const 0
        i32.load)
      (func $main (type $main_t) (param i32) (result i64 i64) 
         i64.const 0
         i64.const 0)
      (export "main" (func $main)))
    |}
  in
  ignore (parse code)

let test_incorrect_syntax () =
  let code = "(module" in
  Alcotest.check_raises "Raises parse error" Parse_error (fun () ->
      ignore (parse code))

let test_memory_size () =
  let code = "(module (memory 65540))" in
  Alcotest.check_raises "Raises validation error" Parse_error (fun () ->
      ignore (parse code))

let test_multiple_memories () =
  let code = "(module (memory 1) (memory 1))" in
  Alcotest.check_raises "Raises validation error" Parse_error (fun () ->
      ignore (parse code))

let test_func_validation () =
  let code = "(module (func $main (result i32) i64.const 0))" in
  Alcotest.check_raises "Raises validation error" Parse_error (fun () ->
      ignore (parse code))

let test =
  let open Alcotest in
  ( "Parsing",
    [
      test_case "Successful parsing" `Quick test_successful_parsing;
      test_case "Error on incorrect syntax" `Quick test_incorrect_syntax;
      test_case "Contract validation (memory size)" `Quick test_memory_size;
      test_case "Contract validation (single memory)" `Quick
        test_multiple_memories;
      test_case "Contract validation (function)" `Quick test_func_validation;
    ] )
