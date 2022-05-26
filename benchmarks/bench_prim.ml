open Core_bench
open Lambda_vm
open Vm_utils

let prim prim = Ast.Prim prim

(* Primivites with 1 parameter *)
let script_op1 prim =
  [%lambda_vm.script fun param -> ([%e prim] param, (0L, 0L))]

(* negation *)
let script_neg = script_op1 (prim Neg)

(* negative n : Int64 (Int64.neg n) *)
let compile_value_neg_lib n ~initial_gas =
  let n = Int64.(neg (Int64.of_int n)) in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let test_compile_value_neg_lib =
  Bench.Test.create_indexed ~name:"compile value neg lib" ~args:[0; 1; 2]
    (fun n ->
      Core.Staged.stage (fun () ->
          let _ = compile_value_neg_lib n ~initial_gas:200 in
          ()))

let test_execute_neg_lib =
  Bench.Test.create_indexed ~name:"execute neg lib" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_neg_lib n ~initial_gas:200 in
          let gas_compile = Gas.make ~initial_gas:1501 in
          let ir = compile_exn gas_compile script_neg in
          let gas = Gas.make ~initial_gas:1501 in
          let _ = execute_exn gas arg ir in
          ()))

(* negative n: Int64 n *)
let compile_value_exn_neg n ~initial_gas =
  let n = Int64.of_int n in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let test_compile_value_neg =
  Bench.Test.create_indexed ~name:"compile value neg" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let _ = compile_value_exn_neg n ~initial_gas:200 in
          ()))

let test_execute_neg =
  Bench.Test.create_indexed ~name:"execute neg" ~args:[0; 1; 2] (fun n ->
      Core.Staged.stage (fun () ->
          let arg = compile_value_exn_neg n ~initial_gas:200 in
          let gas_compile = Gas.make ~initial_gas:1501 in
          let ir = compile_exn gas_compile script_neg in
          let gas = Gas.make ~initial_gas:1501 in
          let _ = execute_exn gas arg ir in
          ()))

(* Primitives with 2 parameteres *)
let script_op2 prim =
  [%lambda_vm.script fun param -> ([%e prim] (fst param) (snd param), (0L, 0L))]

(* addition *)

(* Note: I cannot parse the execute of libary because the compile value of
    Int64.add is (Int64 value) and not a (Pair value) while the script is a pair
    value *)

let script_add = script_op2 (prim Add)

let compile_value_exn_lib f (n1, n2) ~initial_gas =
  let n = f n1 n2 in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let bench_compile_value_pair_lib s f (n1, n2) ~initial_gas =
  let name =
    "compile value "
    ^ s
    ^ " ("
    ^ Int64.to_string n1
    ^ ", "
    ^ Int64.to_string n2
    ^ ")" in
  Bench.Test.create ~name (fun () ->
      let _ = compile_value_exn_lib f (n1, n2) ~initial_gas in
      ())

(* Addition n: Int64 n1 n2 *)
let test_compile_value_add_lib =
  bench_compile_value_pair_lib "add_lib" Int64.add (1L, 2L) ~initial_gas:2000

(* Addition n: pair (n1, n2) *)
let test_compile_value_add =
  bench_compile_value_pair "add" (1L, 2L) ~initial_gas:2000

let test_compile_add =
  bench_compile_script "add" ~initial_gas:2000 ~script:script_add

let test_execute_add =
  bench_execute_exn_pair "add" (1L, 2L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_add

(* subtraction *)
let script_sub = script_op2 (prim Sub)

let test_compile_value_sub =
  bench_compile_value_pair "sub" (2L, 1L) ~initial_gas:2000

let test_compile_value_sub_lib =
  bench_compile_value_pair_lib "sub_lib" Int64.sub (2L, 1L) ~initial_gas:2000

let test_compile_sub =
  bench_compile_script "sub" ~initial_gas:2000 ~script:script_sub

let test_execute_sub =
  bench_execute_exn_pair "sub" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_sub

(* Multiplication *)
let script_mul = script_op2 (prim Mul)

let test_compile_value_mul =
  bench_compile_value_pair "mul" (2L, 1L) ~initial_gas:2000

let test_compile_value_mul_lib =
  bench_compile_value_pair_lib "mul_lib" Int64.mul (2L, 1L) ~initial_gas:2000

let test_compile_mul =
  bench_compile_script "mul" ~initial_gas:2000 ~script:script_mul

let test_execute_mul =
  bench_execute_exn_pair "mul" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_mul

(* Division *)
let script_div = script_op2 (prim Div)

let test_compile_value_div =
  bench_compile_value_pair "div" (2L, 1L) ~initial_gas:2000

let test_compile_value_div_lib =
  bench_compile_value_pair_lib "div_lib" Int64.div (2L, 1L) ~initial_gas:2000

let test_compile_div =
  bench_compile_script "div" ~initial_gas:2000 ~script:script_div

let test_execute_div =
  bench_execute_exn_pair "div" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_div

(* Remainder *)
let script_rem = script_op2 (prim Rem)

let test_compile_value_rem =
  bench_compile_value_pair "rem" (2L, 1L) ~initial_gas:2000

let test_compile_value_rem_lib =
  bench_compile_value_pair_lib "rem_lib" Int64.rem (2L, 1L) ~initial_gas:2000

let test_compile_rem =
  bench_compile_script "rem" ~initial_gas:2000 ~script:script_rem

let test_execute_rem =
  bench_execute_exn_pair "rem" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_rem

(* Bitwise logical and *)
let script_land = script_op2 (prim Land)

let test_compile_value_land =
  bench_compile_value_pair "land" (2L, 1L) ~initial_gas:2000

let test_compile_value_land_lib =
  bench_compile_value_pair_lib "land_lib" Int64.logand (2L, 1L)
    ~initial_gas:2000

let test_compile_land =
  bench_compile_script "land" ~initial_gas:2000 ~script:script_land

let test_execute_land =
  bench_execute_exn_pair "land" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_land

(* Bitwise logical or *)
let script_lor = script_op2 (prim Lor)

let test_compile_value_lor =
  bench_compile_value_pair "lor" (2L, 1L) ~initial_gas:2000

let test_compile_value_lor_lib =
  bench_compile_value_pair_lib "lor_lib" Int64.logor (2L, 1L) ~initial_gas:2000

let test_compile_lor =
  bench_compile_script "lor" ~initial_gas:2000 ~script:script_lor

let test_execute_lor =
  bench_execute_exn_pair "lor" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_lor

(* Bitwise logical exclusive or *)
let script_lxor = script_op2 (prim Lxor)

let test_compile_value_lxor =
  bench_compile_value_pair "lxor" (2L, 1L) ~initial_gas:2000

let test_compile_value_lxor_lib =
  bench_compile_value_pair_lib "lxor_lib" Int64.logxor (2L, 1L)
    ~initial_gas:2000

let test_compile_lxor =
  bench_compile_script "lxor" ~initial_gas:2000 ~script:script_lxor

let test_execute_lxor =
  bench_execute_exn_pair "lxor" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_lxor

(* Shift left *)
let script_lsl = script_op2 (prim Lsl)

let test_compile_value_lsl =
  bench_compile_value_pair "lsl" (2L, 1L) ~initial_gas:2000

let compile_value_exn_shift f n1 n2 ~initial_gas =
  let n = f n1 (Int64.to_int n2) in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let bench_compile_value_shift_lib s f n1 n2 ~initial_gas =
  let name =
    "compile value "
    ^ s
    ^ " ("
    ^ Int64.to_string n1
    ^ ", "
    ^ Int64.to_string n2
    ^ ")" in
  Bench.Test.create ~name (fun () ->
      let _ = compile_value_exn_shift f n1 n2 ~initial_gas in
      ())

let test_compile_value_lsl_lib =
  bench_compile_value_shift_lib "lsl_lib" Int64.shift_left 2L 1L
    ~initial_gas:2000

let test_compile_lsl =
  bench_compile_script "lsl" ~initial_gas:2000 ~script:script_lsl

let test_execute_lsl =
  bench_execute_exn_pair "lsl" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_lsl

(* Shift right logical *)
let script_lsr = script_op2 (prim Lsr)

let test_compile_value_lsr_lib =
  bench_compile_value_shift_lib "lsr_lib" Int64.shift_right_logical 2L 1L
    ~initial_gas:2000

let test_compile_lsr =
  bench_compile_script "lsr" ~initial_gas:2000 ~script:script_lsr

let test_execute_lsr =
  bench_execute_exn_pair "lsr" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_lsr

(* Shift right *)
let script_asr = script_op2 (prim Asr)

let test_compile_value_asr_lib =
  bench_compile_value_shift_lib "asr_lib" Int64.shift_right 2L 1L
    ~initial_gas:2000

let test_compile_asr =
  bench_compile_script "asr" ~initial_gas:2000 ~script:script_asr

let test_execute_asr =
  bench_execute_exn_pair "asr" (2L, 1L) ~gas_value:2000 ~gas_compile:2000
    ~gas_exe:2000 ~script:script_asr

(* Compare time of the:
   - compile value between vm and lib
   - compile scripts
   - execution time;
*)
let tests =
  [
    test_compile_value_neg;
    test_compile_value_neg_lib;
    test_execute_neg;
    test_execute_neg_lib;
    (* compile value primitives 2 parameters *)
    test_compile_value_add;
    test_compile_value_sub;
    test_compile_value_mul;
    test_compile_value_div;
    test_compile_value_rem;
    test_compile_value_land;
    test_compile_value_lor;
    test_compile_value_lxor;
    test_compile_value_lsl;
    test_compile_value_add_lib;
    test_compile_value_sub_lib;
    test_compile_value_mul_lib;
    test_compile_value_div_lib;
    test_compile_value_rem_lib;
    test_compile_value_land_lib;
    test_compile_value_lor_lib;
    test_compile_value_lxor_lib;
    test_compile_value_lsl_lib;
    test_compile_value_lsr_lib;
    test_compile_value_asr_lib;
    (* compile scripts primitives 2 paramters *)
    test_compile_add;
    test_compile_sub;
    test_compile_mul;
    test_compile_div;
    test_compile_rem;
    test_compile_land;
    test_compile_lor;
    test_compile_lxor;
    test_compile_lsl;
    test_compile_lsr;
    test_compile_asr;
    (* execute primitives 2 parameters *)
    test_execute_add;
    test_execute_sub;
    test_execute_mul;
    test_execute_div;
    test_execute_rem;
    test_execute_land;
    test_execute_lor;
    test_execute_lxor;
    test_execute_lsl;
    test_execute_lsr;
    test_execute_asr;
  ]

let command = Bench.make_command tests