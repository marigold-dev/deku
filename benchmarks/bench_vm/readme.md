# Benchmark Lambda Virtual Machine

Computer configuation that ran the benchmark:

```
Memory: 15.3 GiB
Processor: Intel® Core™ i7-8665U CPU @ 1.90GHz × 8 
OS Name: Ubuntu 20.04.4 LTS
OS Type: 64-bit
GNOME version: 3.36.8
```

We are using the micro-benchmarking library [`core_bench`](https://github.com/janestreet/core_bench) to measure the execution costs of operations in Lambda VM. 

We have 3 mains benchmarking functions:
- compile:
    - bench `compile_value` function.
    - bench `compile` function.
- execution function: The combination of 3 functions: `compile_value`, `compile` and `execute`.

## Run
- dune:

    - Build: `dune build @benchmarks/bench`

    - Execute: `dune exec -- ./benchmarks/benchmarks.exe subcommand`

Where `subcommand` is:
- `gas`: for benchmark gas
- `recursive`: for recursive functions
- `prim`: for primitives
- `expr`: for simple expressions

For example: `dune exec -- ./benchmarks/benchmarks.exe gas` will return the benchmark for gas.

- esy:

    - Build: `esy b dune build`

    - Execute: `esy b dune exec ./benchmarks/benchmarks.exe subcommand`

## Understanding the results

- Time: number of nano secs taken
- Run: runs per sampled batch
- mWd: words allocated to the minor heap
- mjWd: words allocated to the major heap
- Prom: promoted words
- Percentage: relative execution time as a percentage of the longest execution time

Let's take this as an example:

```
┌───────────────────────────────┬──────────┬─────────┬────────────┐
│ Name                          │ Time/Run │ mWd/Run │ Percentage │
├───────────────────────────────┼──────────┼─────────┼────────────┤
│ compile value add_lib (1, 2)  │ 115.39ns │  20.00w │     89.37% │
```

- Time/Run `115.39ns`: this mean that the cost of calling a function `compile value add_lib (1, 2)` is estimated to be about `115.39` nanos. 
- mWd/Run `20.00w`: allocates `20.00` words on the minor heap.

The example: 

```
┌──────────────────────────────────┬────────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                             │   Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────────────────┼────────────┼─────────┼──────────┼──────────┼────────────┤
│ execute pair (1, 99) incr_lambda │ 2_951.60ns │ 649.99w │    0.20w │    0.20w │    100.00% │
```

- mjWd/Run `0.20w`: allocates `0.20` words directly into the major heap.
- Prom/Run `0.20w`: promoted words

For more please read: [core_bench](https://github.com/janestreet/core_bench/blob/master/inline_benchmarks_runner_lib_public/bin/runner-help-for-review.org).

## Primitives

Source: `bench_prim.ml`

### Compile value

Below is the benchmark of `compile_value` for primitive functions taken from the OCaml `Int64` library, for instance: `Int64.add` for addition.


```
Estimated testing time 1m50s (11 benchmarks x 10s). Change using '-quota'.
┌───────────────────────────────┬──────────┬─────────┬────────────┐
│ Name                          │ Time/Run │ mWd/Run │ Percentage │
├───────────────────────────────┼──────────┼─────────┼────────────┤
│ compile value add_lib (1, 2)  │ 115.39ns │  20.00w │     89.37% │
│ compile value sub_lib (2, 1)  │  62.39ns │  20.00w │     48.32% │
│ compile value mul_lib (2, 1)  │  78.59ns │  20.00w │     60.87% │
│ compile value div_lib (2, 1)  │ 106.26ns │  20.00w │     82.30% │
│ compile value rem_lib (2, 1)  │ 115.25ns │  20.00w │     89.26% │
│ compile value land_lib (2, 1) │  60.64ns │  20.00w │     46.96% │
│ compile value lor_lib (2, 1)  │ 100.36ns │  20.00w │     77.73% │
│ compile value lxor_lib (2, 1) │  51.87ns │  20.00w │     40.18% │
│ compile value lsl_lib (2, 1)  │ 129.11ns │  17.00w │    100.00% │
│ compile value lsr_lib (2, 1)  │  84.86ns │  17.00w │     65.72% │
│ compile value asr_lib (2, 1)  │ 128.52ns │  17.00w │     99.54% │
└───────────────────────────────┴──────────┴─────────┴────────────┘
```

### Compile script

Benchmark for function `compile` primitives in VM, for instance: script add is the `Ast.Prim Add`.

```
Estimated testing time 1m50s (11 benchmarks x 10s). Change using '-quota'.
┌─────────────────────┬──────────┬─────────┬────────────┐
│ Name                │ Time/Run │ mWd/Run │ Percentage │
├─────────────────────┼──────────┼─────────┼────────────┤
│ compile script add  │ 814.07ns │ 158.01w │    100.00% │
│ compile script sub  │ 684.34ns │ 157.98w │     84.06% │
│ compile script mul  │ 606.04ns │ 158.02w │     74.45% │
│ compile script div  │ 598.40ns │ 157.99w │     73.51% │
│ compile script rem  │ 464.44ns │ 158.00w │     57.05% │
│ compile script land │ 576.66ns │ 158.00w │     70.84% │
│ compile script lor  │ 647.40ns │ 158.02w │     79.53% │
│ compile script lxor │ 407.93ns │ 157.99w │     50.11% │
│ compile script lsl  │ 365.67ns │ 158.00w │     44.92% │
│ compile script lsr  │ 314.95ns │ 157.99w │     38.69% │
│ compile script asr  │ 496.81ns │ 158.01w │     61.03% │
└─────────────────────┴──────────┴─────────┴────────────┘
```

### Interpreter

```
Estimated testing time 1m50s (11 benchmarks x 10s). Change using '-quota'.
┌──────────────────────────┬──────────┬─────────┬────────────┐
│ Name                     │ Time/Run │ mWd/Run │ Percentage │
├──────────────────────────┼──────────┼─────────┼────────────┤
│ execute pair (1, 2) add  │   1.81us │ 344.05w │     54.78% │
│ execute pair (2, 1) sub  │   2.45us │ 344.04w │     74.10% │
│ execute pair (2, 1) mul  │   1.58us │ 344.02w │     47.98% │
│ execute pair (2, 1) div  │   3.30us │ 343.92w │    100.00% │
│ execute pair (2, 1) rem  │   1.86us │ 343.95w │     56.36% │
│ execute pair (2, 1) land │   2.78us │ 343.98w │     84.07% │
│ execute pair (2, 1) lor  │   2.04us │ 344.03w │     61.86% │
│ execute pair (2, 1) lxor │   1.32us │ 344.02w │     40.08% │
│ execute pair (2, 1) lsl  │   2.36us │ 354.05w │     71.51% │
│ execute pair (2, 1) lsr  │   1.67us │ 354.04w │     50.63% │
│ execute pair (2, 1) asr  │   3.29us │ 354.02w │     99.63% │
└──────────────────────────┴──────────┴─────────┴────────────┘
```

## Expression function

Benchmark of several simple expressions. Source `bench_simple_expression.ml`.

### Compile value

These are the results of `compile_value`. When it is a number for instance `43` it is a `Int64 43L` in VM, when it is for instance `pair (1, 51)`, it is a `Pair (Int64 1L, Int64 51L)` in VM.

```
Estimated testing time 2m10s (13 benchmarks x 10s). Change using '-quota'.
┌────────────────────────────────────────┬──────────┬─────────┬────────────┐
│ Name                                   │ Time/Run │ mWd/Run │ Percentage │
├────────────────────────────────────────┼──────────┼─────────┼────────────┤
│ compile value incr 43                  │  44.37ns │  14.00w │     25.79% │
│ compile value decr 41                  │  36.76ns │  14.00w │     21.37% │
│ compile value pair 51                  │  36.79ns │  14.00w │     21.39% │
│ compile value pair (23, 28) pair       │ 146.24ns │  39.00w │     85.01% │
│ compile value if_true 52               │  43.89ns │  14.00w │     25.51% │
│ compile value pair (1, 51) if_true     │ 172.03ns │  39.00w │    100.00% │
│ compile value if_false 32              │  69.71ns │  14.00w │     40.52% │
│ compile value pair (0, 33) if_false    │ 118.20ns │  39.00w │     68.71% │
│ compile value incr_lambda 100          │  47.55ns │  14.00w │     27.64% │
│ compile value pair (1, 99) incr_lambda │ 113.13ns │  39.00w │     65.76% │
│ compile value decr_lambda 32           │  66.43ns │  14.00w │     38.62% │
│ compile value pair (0, 33) decr_lambda │ 166.31ns │  39.00w │     96.67% │
│ compile value lam_app 45               │  71.00ns │  14.00w │     41.27% │
└────────────────────────────────────────┴──────────┴─────────┴────────────┘
```

### Compile script

```
Estimated testing time 1m (6 benchmarks x 10s). Change using '-quota'.
┌───────────────────────────┬────────────┬─────────┬────────────┐
│ Name                      │   Time/Run │ mWd/Run │ Percentage │
├───────────────────────────┼────────────┼─────────┼────────────┤
│ compile script incr       │   236.77ns │ 114.00w │     15.39% │
│ compile script decr       │   501.17ns │ 114.01w │     32.58% │
│ compile script pair       │   472.01ns │ 158.00w │     30.68% │
│ compile script if         │ 1_379.42ns │ 255.03w │     89.67% │
│ compile script lambda     │ 1_538.24ns │ 358.01w │    100.00% │
│ compile script lambda_app │   710.75ns │ 121.01w │     46.21% │
└───────────────────────────┴────────────┴─────────┴────────────┘
```

### Interpreter

```
Estimated testing time 1m20s (8 benchmarks x 10s). Change using '-quota'.
┌──────────────────────────────────┬────────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                             │   Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────────────────┼────────────┼─────────┼──────────┼──────────┼────────────┤
│ execute incr                     │ 1_256.41ns │ 241.02w │          │          │     42.57% │
│ execute decr                     │   612.63ns │ 240.99w │          │          │     20.76% │
│ execute pair (23, 28) pair       │ 1_920.00ns │ 344.03w │          │          │     65.05% │
│ execute pair (1, 51) if_true     │ 1_679.32ns │ 456.97w │          │          │     56.90% │
│ execute pair (0, 33) if_false    │ 1_648.00ns │ 457.01w │          │          │     55.83% │
│ execute pair (1, 99) incr_lambda │ 2_951.60ns │ 649.99w │    0.20w │    0.20w │    100.00% │
│ execute pair (0, 33) decr_lambda │ 2_608.19ns │ 649.95w │    0.20w │    0.20w │     88.37% │
│ execute lambda_app               │ 1_170.26ns │ 245.99w │          │          │     39.65% │
└──────────────────────────────────┴────────────┴─────────┴──────────┴──────────┴────────────┘
```

## Recursive functions

Recursive functions: factorial and fibonacci. Source `bench_recursion.ml`.

The benchmarks using the `Bench.Test.create_indexed`. It is a group of benchmarks indexed by size, 
for instance `[1;2;3]` in the example of factorial below.


### Compile value

```
Estimated testing time 1m (6 benchmarks x 10s). Change using '-quota'.
┌───────────────────────────┬──────────┬─────────┬────────────┐
│ Name                      │ Time/Run │ mWd/Run │ Percentage │
├───────────────────────────┼──────────┼─────────┼────────────┤
│ compile value factorial:1 │ 503.14ns │ 252.99w │     76.29% │
│ compile value factorial:2 │ 659.54ns │ 318.99w │    100.00% │
│ compile value factorial:3 │ 642.05ns │ 385.02w │     97.35% │
│ compile value fibonacci:0 │  61.18ns │  17.00w │      9.28% │
│ compile value fibonacci:1 │  38.44ns │  17.00w │      5.83% │
│ compile value fibonacci:2 │  82.39ns │  26.00w │     12.49% │
└───────────────────────────┴──────────┴─────────┴────────────┘
```

### Compile script

```
Estimated testing time 20s (2 benchmarks x 10s). Change using '-quota'.
┌──────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                     │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ compile script factorial │   2.86us │ 391.07w │          │          │     78.92% │
│ compile script fibonacci │   3.62us │ 606.95w │    0.22w │    0.22w │    100.00% │
└──────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
```

### Interpreter

```
Estimated testing time 1m (6 benchmarks x 10s). Change using '-quota'.
┌─────────────────────┬──────────┬───────────┬──────────┬──────────┬────────────┐
│ Name                │ Time/Run │   mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├─────────────────────┼──────────┼───────────┼──────────┼──────────┼────────────┤
│ execute factorial:1 │   6.80us │   822.89w │    0.29w │    0.29w │     48.03% │
│ execute factorial:2 │   5.39us │ 1_020.95w │    0.39w │    0.39w │     38.02% │
│ execute factorial:3 │  14.17us │ 1_795.01w │    0.92w │    0.92w │    100.00% │
│ execute fibonacci:0 │   3.56us │   972.89w │    0.43w │    0.43w │     25.15% │
│ execute fibonacci:1 │   6.84us │   973.01w │    0.43w │    0.43w │     48.27% │
│ execute fibonacci:2 │   6.42us │ 1_586.17w │    0.90w │    0.90w │     45.32% │
└─────────────────────┴──────────┴───────────┴──────────┴──────────┴────────────┘
```

## Gas

Source `bench_gas.ml`

### Interpreter

Using the counter script as an example, keep the initial gas of `compile` and `execute`.

```
let counter_script =
  [%lambda_vm.script
    fun x ->
      ( (fun f -> f f x) (fun f n ->
            if n then
              1L + f f (n - 1L)
            else
              0L),
        (0L, 0L) )]
```

Increase 100 times initial gas for each compile value (from 101 to 1001).

```
Estimated testing time 1m40s (10 benchmarks x 10s). Change using '-quota'.
┌───────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                  │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ execute counter_gas_0 │   4.74us │ 620.00w │    0.18w │    0.18w │     85.51% │
│ execute counter_gas_1 │   3.54us │ 620.00w │    0.18w │    0.18w │     63.88% │
│ execute counter_gas_2 │   3.20us │ 619.92w │    0.18w │    0.18w │     57.77% │
│ execute counter_gas_3 │   2.34us │ 619.98w │    0.18w │    0.18w │     42.19% │
│ execute counter_gas_4 │   2.04us │ 619.99w │    0.18w │    0.18w │     36.83% │
│ execute counter_gas_5 │   2.89us │ 619.99w │    0.18w │    0.18w │     52.10% │
│ execute counter_gas_6 │   2.17us │ 620.07w │    0.18w │    0.18w │     39.09% │
│ execute counter_gas_7 │   5.55us │ 620.02w │    0.18w │    0.18w │    100.00% │
│ execute counter_gas_8 │   3.14us │ 619.91w │    0.18w │    0.18w │     56.59% │
│ execute counter_gas_9 │   4.92us │ 620.13w │    0.18w │    0.18w │     88.65% │
└───────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
```

Increase 1000 times initial gas for each compile value (from 2001 to 10_001).

```
Estimated testing time 1m30s (9 benchmarks x 10s). Change using '-quota'.
┌────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name                   │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ execute counter_gas_10 │   4.67us │ 620.08w │    0.18w │    0.18w │     97.29% │
│ execute counter_gas_11 │   3.16us │ 619.92w │    0.18w │    0.18w │     65.85% │
│ execute counter_gas_12 │   4.77us │ 620.04w │    0.18w │    0.18w │     99.32% │
│ execute counter_gas_13 │   2.60us │ 620.11w │    0.18w │    0.18w │     54.22% │
│ execute counter_gas_14 │   4.33us │ 619.92w │    0.18w │    0.18w │     90.15% │
│ execute counter_gas_15 │   3.46us │ 620.04w │    0.18w │    0.18w │     72.02% │
│ execute counter_gas_16 │   4.80us │ 619.97w │    0.18w │    0.18w │    100.00% │
│ execute counter_gas_17 │   3.14us │ 619.95w │    0.18w │    0.18w │     65.36% │
│ execute counter_gas_18 │   3.21us │ 619.93w │    0.18w │    0.18w │     66.82% │
└────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
```

From the results:
- The `mWd/Run` does not have a big different when given different gas value.
- The value in `mjWd/Run`, and `Prom/Run` has the same value.