# Benchmark of Lambda VM by using the library ocaml-benchmarks

[OCaml benchmark](https://github.com/Chris00/ocaml-benchmark) library provides functions to measure and compare the run-time of functions. It is inspired by the Perl module of the same name. 

## Run

- esy:
    - Build: `esy x dune build`
    - Executue: `esy x dune exec ~/deku/_build/default/benchmarks/ocaml_benchmarks/lambda_vm/*.exe` 
    or using alias:
    TODO

- script: TODO

## Understanding the results

TODO

## Primitives

Source `bench_prim.ml`

### Compile value

TODO: remove: result of the first commit example

```
~/deku/benchmarks/ocaml_benchmarks/lambda_vm$ esy x dune exec ~/deku/_build/default/benchmarks/ocaml_benchmarks/lambda_vm/bench_prim.exe
Entering directory '/home/quyen/deku'  
Throughputs for "n=1" running 5 times for at least 10 CPU seconds:
n=1: 10.46 WALL (10.45 usr +  0.00 sys = 10.45 CPU) @ 47793454.06/s (n=499572549)
     10.18 WALL (10.15 usr +  0.01 sys = 10.16 CPU) @ 49983135.01/s (n=507961157)
     10.09 WALL (10.08 usr +  0.00 sys = 10.09 CPU) @ 52539339.11/s (n=529879988)
     10.02 WALL (10.02 usr +  0.00 sys = 10.02 CPU) @ 53116111.30/s (n=531969434)
     10.15 WALL (10.14 usr +  0.00 sys = 10.14 CPU) @ 49282260.42/s (n=499572549)

          Rate          n=1
n=1 50542860+-1903284/s  --
Latencies for 20000 iterations of "n=1":
n=1:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 49504950.50/s (n=20000)
     (warning: too few iterations for a reliable count)

          Rate n=1
n=1 49504950/s  --
```

### Compile script

TODO

### Iterpreter

TODO