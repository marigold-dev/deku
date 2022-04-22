open Lambda_vm

let sender =
  "tz1ibMpWS6n6MJn73nQHtK5f4ogyYC1z9T9z" |> Core.Address.of_string |> Option.get

let factorial =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n -> if n then n * f f (n - 1L) else 1L),
        (0L, 0L) )]

let test_factorial =
  let rec fac = function
    | 0L -> 1L
    | n -> Int64.(mul n (fac (sub n 1L))) in
  QCheck_alcotest.to_alcotest
    QCheck.(
      Test.make ~name:"Recursion with factorial" ~count:10000 (1 -- 26)
        (fun x ->
          (* Less than 0 is infinite recursion, greater than 25 is integer overflow. *)
          let x = Int64.of_int x in
          let result =
            Vm_test.execute_ast_exn sender 1_000_000 (Int64 x) factorial in
          let expected_result =
            Vm_test.compile_value_exn
              (Gas.make ~initial_gas:101)
              (Int64 (fac x)) in
          expected_result = result.storage))

let fibonacci =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if (0L - n) * (1L - n) then
              f f (n - 2L) + f f (n - 1L)
            else
              1L),
        (0L, 0L) )]

let test_fibonacci =
  let rec fib = function
    | 0L
    | 1L ->
      1L
    | n -> Int64.add (fib (Int64.sub n 1L)) (fib (Int64.sub n 2L)) in
  QCheck_alcotest.to_alcotest
    QCheck.(
      Test.make ~name:"Fibonacci" ~count:100 (0 -- 25) (fun x ->
          let x = Int64.of_int x in
          let result =
            Vm_test.execute_ast_exn sender 100000000000 (Int64 x) fibonacci
          in
          let expected_value =
            Vm_test.compile_value_exn
              (Gas.make ~initial_gas:101)
              (Int64 (fib x)) in
          expected_value = result.storage))

let counter =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if n then
              1L + f f (n - 1L)
            else
              0L),
        (0L, 0L) )]

let test_counter =
  QCheck_alcotest.to_alcotest
    QCheck.(
      Test.make ~name:"Counter" ~count:1000 (0 -- 10000) (fun x ->
          let x = Int64.of_int x in
          let result =
            Vm_test.execute_ast_exn sender 1000000000 (Int64 x) counter in
          let expected_value =
            Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 x)
          in
          expected_value = result.storage))

let check_runtime_error ~msg ~actual ~expected =
  let open Vm_test in
  match actual with
  | Error
      ( Compilation_error (Runtime_limits_error error)
      | Execution_error (Runtime_limits_error error) ) ->
    Alcotest.(check Testable.runtime_limits_error) msg expected error
  | Ok _ -> Alcotest.fail "Ast shouldn't execute"
  | _ -> Alcotest.fail "unexpected error"
let test_stack_limit () =
  check_runtime_error ~msg:"Stack has a limit" ~expected:Out_of_stack
    ~actual:
      (Vm_test.execute_ast sender 71_990_801
         (Int64 19996L) (* Bare minimum close to the limit of 20k *)
         counter)

let infinite_recursion_y =
  [%lambda_vm.script fun _ -> (fun f -> f f) (fun f -> f f + 0L)]

let test_y_combinator () =
  check_runtime_error ~msg:"Stack limit avoids infinite recursion"
    ~expected:Out_of_stack
    ~actual:
      (Vm_test.execute_ast sender 10000000000000000 (Int64 0L)
         infinite_recursion_y)

let infinite_recursion_z =
  [%lambda_vm.script fun _ -> (fun f -> f f 0L) (fun f v -> f f (v + 0L))]

let test_z_combinator () =
  check_runtime_error ~msg:"Gas limit is triggered" ~expected:Out_of_gas
    ~actual:
      (Vm_test.execute_ast sender 10000000000 (Int64 0L) infinite_recursion_z)

let test =
  let open Alcotest in
  ( "Recursion",
    [
      test_factorial;
      test_fibonacci;
      test_counter;
      test_case "Stack limit" `Slow test_stack_limit;
      test_case "Infinite recursion - Y" `Slow test_y_combinator;
      test_case "Infinite recursion - Z" `Slow test_z_combinator;
    ] )
