(*
  https://github.com/Chris00/ocaml-benchmark/blob/master/examples/numbers.ml
  Doc: https://chris00.github.io/ocaml-benchmark/doc/benchmark/Benchmark/index.html

  Build by esy:
  - ~/deku$ esy x dune build
  Run the benchmarks:
  ~/deku$ esy x dune exec ~/deku/_build/default/benchmarks/ocaml_benchmarks/numbers.exe
*)

open Printf
open Benchmark

(* Test the speed of addition for native ints (unboxed), and Int32/Int64  (which are both boxed). *)

let f_int n =
  let rec loop i sum =
    if i < n then
      loop (i + 1) (sum + 1)
    else
      sum in
  loop 0 0

let f_int32 n =
  let rec loop i sum =
    if i < n then
      loop (i + 1) (Int32.add sum Int32.one)
    else
      sum in
  Int32.to_int (loop 0 Int32.zero)

let f_int64 n =
  let rec loop i sum =
    if i < n then
      loop (i + 1) (Int64.add sum Int64.one)
    else
      sum in
  Int64.to_int (loop 0 Int64.zero)

(* Main function for benchmarks *)
let () =
  (* print out the results of the f_* functions to doublecheck that
     they work as we intend *)
  printf "f_int 777 = %d\n" (f_int 777);
  printf "f_int32 777 = %d\n" (f_int32 777);
  printf "f_int64 777 = %d\n" (f_int64 777);
  print_newline ();

  (* let's exercise the *1 functions:
     - 10_000: is the n in f_int n (input argument of the function to test)
     - 1000L: mean Latencies for 1000 iterations
     - 5: mean each function running for at least 5 CPU seconds
     note that, it is only testing the functon f_int.
  *)
  let _ = latency1 ~name:"int-1-latency1" 1000L f_int 10_000 in
  let _ = throughput1 ~name:"int-1-throughput1" 5 f_int 10_000 in
  print_newline ();

  (* let's exercise the *N functions:  *)
  (* the part for benchmark with throughputN
     - 5: each running 5 times
     - 10: each running at least 10 CPU seconds
  *)
  let res =
    throughputN ~repeat:5 10
      [
        ("int", f_int, 10_000);
        ("int32", f_int32, 10_000);
        ("int64", f_int64, 10_000);
      ] in
  print_newline ();
  tabulate res;

  (* the part for benchmark latencyN *)
  print_newline ();
  let res =
    latencyN 2000L
      [
        ("int", f_int, 10_000);
        ("int32", f_int32, 10_000);
        ("int64", f_int64, 10_000);
      ] in
  print_newline ();
  tabulate res
