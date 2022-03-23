open Lambda_vm

module Testable = Vm_test.Testable

let script_op2 prim =
  let prim = Ast.Prim prim in
  [%lambda_vm.script fun param -> ([%e prim] (fst param) (snd param), (0L, 0L))]

let script_op1 prim =
  let prim = Ast.Prim prim in
  [%lambda_vm.script fun param -> ([%e prim] param, (0L, 0L))]

let make_op2_test ~name prim f =
  QCheck_alcotest.to_alcotest
    QCheck.(
      Test.make ~name ~count:10_000 (pair int64 int64) (fun (a, b) ->
          let result =
            Vm_test.execute_ast_exn 2901
              (Pair (Int64 a, Int64 b))
              (script_op2 prim) in
          let expected_result =
            Vm_test.compile_value_exn
              (Gas.make ~initial_gas:200)
              (Int64 (f a b)) in
          result.storage = expected_result))

let test_sum = make_op2_test ~name:"Adding pairs" Add Int64.add

let test_sub = make_op2_test ~name:"Subtracting pairs" Sub Int64.sub

let test_mul = make_op2_test ~name:"Multiplying pairs" Mul Int64.mul

let test_div = make_op2_test ~name:"Dividing pairs" Div Int64.div

let test_rem = make_op2_test ~name:"Modulo pairs" Rem Int64.rem

let test_and = make_op2_test ~name:"Anding pairs" Land Int64.logand

let test_or = make_op2_test ~name:"Oring pairs" Lor Int64.logor

let test_xor = make_op2_test ~name:"Xoring pairs" Lxor Int64.logxor

let test_lsl =
  make_op2_test ~name:"Left shifting pairs" Lsl (fun a b ->
      Int64.shift_left a (Int64.to_int b))

let test_lsr =
  make_op2_test ~name:"Logical right shifting pairs" Lsr (fun a b ->
      Int64.shift_right_logical a (Int64.to_int b))

let test_asr =
  make_op2_test ~name:"Right shifting pairs" Asr (fun a b ->
      Int64.shift_right a (Int64.to_int b))

let test_neg =
  QCheck_alcotest.to_alcotest
    QCheck.(
      Test.make ~name:"Negative numbers" ~count:10_000 int64 (fun x ->
          let result = Vm_test.execute_ast_exn 1501 (Int64 x) (script_op1 Neg) in
          let expected_result =
            Vm_test.compile_value_exn
              (Gas.make ~initial_gas:200)
              (Int64 Int64.(neg x)) in
          result.storage = expected_result))

let test =
  ( "Primitive operations",
    [
      test_sum;
      test_sub;
      test_mul;
      test_div;
      test_rem;
      test_and;
      test_or;
      test_xor;
      test_neg;
      test_lsl;
      test_lsr;
      test_asr;
    ] )
