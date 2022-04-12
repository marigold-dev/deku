open Lambda_vm
open Lambda_vm_tests
open Primitives_tests
open Benchmark

(* negative n : Int64 (Int64.neg n),
   give an initial gas as 200 *)

let compile_value_neg_lib n ~initial_gas =
  let n = Int64.(neg (Int64.of_int n)) in
  let gas = Gas.make ~initial_gas in
  compile_value_exn gas (Int64 n)

let compile_neg_lib script =
  let gas_compile = Gas.make ~initial_gas:1501 in
  compile_exn gas_compile script

let execute_neg_lib n script =
  let arg = compile_value_neg_lib n ~initial_gas:200 in
  let gas_compile = Gas.make ~initial_gas:1501 in
  let ir = compile_exn gas_compile script in
  let gas = Gas.make ~initial_gas:1501 in
  execute_exn gas arg ir

(**********************************************************)
(*
    TODO: change the repeat times, iterations in latencyN, etc.
    TODO: in readme, write details about the senario and input values
    for these benchmarks: n, initial gas, etc.
*)

(* benchmark compile value for negative *)
let bench_compile_value_neg s () =
  let list_bench =
    [
      ( "compile value: n=0",
        (fun () -> compile_value_neg_lib ~initial_gas:200 0),
        () );
      ( "compile value: n=1",
        (fun () -> compile_value_neg_lib ~initial_gas:200 1),
        () );
      ( "compile value: n=2",
        (fun () -> compile_value_neg_lib ~initial_gas:200 2),
        () );
    ] in
  (* benchmark for throughputN repeat 5 times, in 10 CPU seconds *)
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline s;
  tabulate res;

  (* benchmark for latencyN:
     - 20000L: latency for 2000 iterations *)
  let res = latencyN 20000L list_bench in
  print_newline ();
  tabulate res

(* benchmark compile for negative *)
let bench_compile_neg s () =
  let list_bench =
    [("compile: negative", (fun () -> compile_neg_lib script_neg), ())] in
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline s;
  tabulate res;

  let res = latencyN 20000L list_bench in
  print_newline ();
  tabulate res

(* benchmark iterpreter for negative *)

let bench_execute_neg s () =
  let list_bench =
    [
      ("execute: n=0", (fun () -> execute_neg_lib 0 script_neg), ());
      ("execute: n=1", (fun () -> execute_neg_lib 1 script_neg), ());
      ("execute: n=2", (fun () -> execute_neg_lib 2 script_neg), ());
    ] in
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline s;
  tabulate res;

  let res = latencyN 20000L list_bench in
  print_newline ();
  tabulate res

let bench_negative () =
  bench_compile_value_neg
    "Compile value negative primitive with n = 0, 1, 2; init_gas = 200" ();
  bench_compile_neg "Compile negative primitive script, init_gas = 1501" ();
  bench_execute_neg "Execute negative primitive with n = 0, 1, 2" ()

(************************************************************************)
(* Primitives with 2 parameters*)

let compile_value_pair (n1, n2) ~initial_gas =
  let gas_value = Gas.make ~initial_gas in
  compile_value_exn gas_value (Pair (Int64 n1, Int64 n2))

let execute_script_exn_pair (n1, n2) ~gas_value ~gas_compile ~gas_exe ~script =
  let arg = compile_value_pair (n1, n2) ~initial_gas:gas_value in
  let gas_ir = Gas.make ~initial_gas:gas_compile in
  let ir = compile_exn gas_ir script in
  let gas = Gas.make ~initial_gas:gas_exe in
  execute_exn gas arg ir

let bench_execute_two_parameters () =
  let list_bench =
    [
      ( "add",
        (fun () ->
          execute_script_exn_pair (1L, 2L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_add),
        () );
      ( "sub",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_sub),
        () );
      ( "mul",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_mul),
        () );
      ( "div",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_div),
        () );
      ( "rem",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_rem),
        () );
      ( "land",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_land),
        () );
      ( "lor",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_lor),
        () );
      ( "lxor",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_lxor),
        () );
      ( "lsl",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_lsl),
        () );
      ( "lsr",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_lsr),
        () );
      ( "asr",
        (fun () ->
          execute_script_exn_pair (2L, 1L) ~gas_value:2000 ~gas_compile:2000
            ~gas_exe:2000 ~script:script_asr),
        () );
    ] in
  let res = throughputN ~repeat:5 10 list_bench in
  print_newline ();
  print_endline "Benchmark two parameters";
  tabulate res;

  let res = latencyN 20_000L list_bench in
  print_newline ();
  tabulate res

let benchmark_prim () =
  bench_negative ();
  bench_execute_two_parameters ()
