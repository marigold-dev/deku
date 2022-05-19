exception Parse_error

let parse code =
  match Wasm_vm.Contract.make ~storage:Bytes.empty ~code with
  | Ok contract -> contract
  | Error msg ->
    Printf.eprintf "%s\n" msg;
    raise Parse_error

let test_successful_parsing () =
  let code =
    {|
    (module
      (memory $mem 1)
      (export "memory" (memory $mem))
      (type $sum_t (func (param i32 i32) (result i32)))
      (func $sum_f (type $sum_t) (param $x i32) (param $y i32) (result i32)
        i32.const 0
        local.get $x
        local.get $y
        i32.add
        i32.store
        i32.const 0
        i32.load)
      (export "sum" (func $sum_f)))
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
