open Lambda_vm

let compare =
  [%lambda_vm
    let delta f = f f in
    let aux aux size a b =
      if size then
        if fst a - fst b then 0L else aux aux (size - 1L) (snd a) (snd b)
      else
        1L in
    let aux = delta aux in
    fun a b ->
      let size_a = fst a in
      let size_b = fst b in
      if size_a - size_b then 0L else aux size_a (snd a) (snd b)]

let code =
  [%lambda_vm.script
    fun address -> ([%e compare] address (sender 0L), (0L, 0L))]

let test_sender () =
  let address = "tz1ibMpWS6n6MJn73nQHtK5f4ogyYC1z9T9z" in
  let result =
    let sender = address |> Core.Address.of_string |> Option.get in
    let gas = Gas.make ~initial_gas:10000 in
    Vm_test.execute_ast_exn sender 1000000
      (Ast.value_of_string gas address)
      code in
  let expected =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:100) [%lambda_vm.value 1L]
  in
  Alcotest.check Vm_test.Testable.value "Same value" expected result.storage

let test = Alcotest.("Test IO", [test_case "Sender" `Quick test_sender])
