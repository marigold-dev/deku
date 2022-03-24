open Lambda_vm

let counter =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if n then
              1L + f f (n - 1L)
            else
              0L),
        (0L, 0L) )]

let test_compile_value () =
  let gas = Gas.make ~initial_gas:100 in
  let _ = Vm_test.compile_value_exn gas (Int64 0L) in
  Alcotest.(check bool) "Should be empty" (Gas.is_empty gas) true
let test_compile_ast () =
  let ast = [%lambda_vm.script fun x -> x + 1L] in

  let gas = Gas.make ~initial_gas:500 in
  let _ = Vm_test.compile_exn gas ast in
  Alcotest.(check bool) "Should be empty" (Gas.is_empty gas) true

let test_execute_ir () =
  let x = 4096L in
  let arg =
    let gas = Gas.make ~initial_gas:100 in
    Vm_test.compile_value_exn gas (Int64 x) in
  let ir =
    let gas = Gas.make ~initial_gas:5000 in
    Vm_test.compile_exn gas counter in

  let gas = Gas.make ~initial_gas:14747900 in
  let _ = Vm_test.execute_exn gas arg ir in
  Alcotest.(check bool) "Should be empty" (Gas.is_empty gas) true
let test =
  let open Alcotest in
  ( "Gas model",
    [
      test_case "Compile value" `Quick test_compile_value;
      test_case "Compile AST" `Quick test_compile_ast;
      test_case "Execute IR" `Quick test_execute_ir;
    ] )
