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

let%expect_test _ =
  let gas = Gas.measure () in
  let _ =
    Vm_test.compile_value_exn gas (Int64 0L) in
  ();
  print_int (Gas.current gas);
  [%expect{| 100 |}]

let%expect_test _ =
  let ast = [%lambda_vm.script fun x -> x + 1L] in

  let gas = Gas.measure () in
  let _ =
    Vm_test.compile_exn gas ast in
  ();
  print_int (Gas.current gas);
  [%expect{| 500 |}]

let%expect_test _ =
  let x = 4096L in
  let arg =
    let gas = Gas.measure () in
    Vm_test.compile_value_exn gas (Int64 x) in
  let ir =
    let gas = Gas.measure () in
    Vm_test.compile_exn gas counter in

  let gas = Gas.measure () in
  let _ =
    Vm_test.execute_exn gas arg ir in
  ();
  print_int (Gas.current gas);
  [%expect{| 14747900 |}]
