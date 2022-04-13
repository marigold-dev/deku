open Benchmark

let bench_throughput_latency s ~repeat ~time:n list_bench ~latency:m =
  let res = throughputN ~repeat n list_bench in
  print_newline ();
  print_endline s;
  tabulate res;

  let res = latencyN m list_bench in
  print_newline ();
  tabulate res
