# Benchmark of Lambda VM by using the library ocaml-benchmark

[OCaml benchmark](https://github.com/Chris00/ocaml-benchmark) library provides functions to measure and compare the run-time of functions. It is inspired by the Perl module of the same name. 


Computer configuation that ran the benchmark:

```
Memory: 16 GiB
Processor: Intel® Core™ i7-8665U CPU @ 1.90GHz × 8 
OS Name: Ubuntu Jammy Jellyfish 
OS Type: 64-bit
GNOME version: 42.0
```

We are using the library [`ocaml-benchmark`](https://github.com/Chris00/ocaml-benchmark) to measure the execution costs of operations in Lambda VM. 

We have 1 main benchmarking function:
- execution function: The combination of 3 functions: `compile_value`, `compile` and `execute`.

## Run

- esy:
    - Build: `esy x dune build`
    - Executue: `esy x dune exec ~/deku/_build/default/benchmarks/time_bench/bench_vm/bench_time_vm.exe` 
    or using alias:
    TODO

- script: TODO

## Understanding the results

[Benchmark documentation](https://chris00.github.io/ocaml-benchmark/doc/benchmark/Benchmark/index.html)

### Throughput

The `throughtputN` and its parameters that used in the benchmark.

`val throughtputN: ?repeat:int -> int -> (string * ('a -> 'b) * 'a) list -> samples`

- `repeat`: the number of times each function running time is measured. 

For example:
```
Throughputs for "make_ticket", "make_address", "make_tezos_address" each running 5 times for at least 10 CPU seconds:
```

mean the functions: `make_ticket`, `make_address`, `make_tezos_address` each function is running 5 times.

- `t: int`: is the running time of the functions, not of the repetition loop.
As the example above, t is 10 seconds. If one is only interested in the relative times of fast functions and not in their real running times, it is recommanded that wrap each of the function in a loop. Because a very fast running function will need a lots of repetition to make a difference of `t` seconds to the empty loop. In this case, the running time of the loop will dominate the whole process which can be therefore take much longer time than `t` seconds.

### Timing and samples structures

```
type t = {
wall : float;	(** Wallclock time (in seconds) *)
utime : float;	(** This process User CPU time (in seconds) *)
stime : float;	(** This process System CPU time (in seconds) *)
cutime : float; (** Child process User CPU time (in seconds) *)
cstime : float; (** Child process System CPU time (in seconds) *)
iters : Int64.t;  (** Number of iterations. *)
}
The information returned by timing tests.
```

The output of the option with `no_child` is as follow:
```
name: t1 WALL (t2 usr + t3 sys = t4 CPU) @ i/s (n=#)
```

For example:
```
n=1: 10.46 WALL (10.45 usr +  0.00 sys = 10.45 CPU) @ 47793454.06/s (n=499572549)
```

Where:
- t1: `wall` is the wallclock time in seconds. In the example it is 10.46 wallclock time seconds.
- t2: `utime` is the process user CPU time in seconds. It is 10.45 user CPU.
- t3: `stime` is the process system CPU time in seconds. It is 0 system CPU.
- t4:  is the cpu_process (`utime + stime`). It is 10.45/s (10.45 + 0) for cpu_process.
- i: is the `iters / cpu_process`. It is 47793454.06.
- n=#: `iters` is the number of iterations. It is 499572549 iterations.

- `samples`: association list that links the names of the tests to the list of their timings.

### Latency

The `latencyN` and its parameters that used in the benchmark.

`val latencyN: ?repeat:int -> Int64.t -> (string * ('a -> 'b) * 'a) list -> samples`

This function runs each function in list `funs` for `n` iterations. `n` must be at least 4.

- `repeat`: number of times each function running time is meansured. It is hightly recommended to set it to a higher number to enable confidence statistics to be performed by `Benchmark.tabulate`.

### Tabulate
`tabulate results` function prints a comparision table for a list of `results` obtained by `latencyN` or `throughputN` wich each function compare to all of the others. The table is of the type

```
Rate         name1 name2 ...   
name1  #/s   --    r12        
name2  #/s   r21    --        
...         ...
```

For example:

```
Rate                             hash_values   hash_tree    add_find
hash_values  287164+- 12780/s          --        -22%        -92%
  hash_tree  368890+- 10443/s         28%          --        -90%
   add_find 3610068+-317285/s       1157%        879%          --

```

where:
- name1, name2: are the labels of the tests sorted from slowest to fastest. As in the example: name1 is `hash_values`, name2 is `hash_tree` and name3 is `add_find`. `hash_values` is the fastest and `add_find` is the slowest.

- rij: says how much name_i is faster (or slower if < 0) than name_j (technically it is equaly to (ri - rj) expressed in percents of rj where r_i and r_j are the rates of name_i and name_j respectively). As in the example: at r12 (row 1 `hash_values`, column 2 `hash_tree`), r12 is `22%`. It means that, `hash_values` is faster than `hash_tree` 22%.

If r_i and r_j are not belived to be different, rij will be printed between brakets as the example below:

```
 Rate                       transfer 4 transfer 1
transfer 4 15250+- 701/s         --      [-1%]
transfer 1 15477+-1112/s       [1%]         --
```

## Primitives

Source `bench_time_prim.ml`

### Iterpreter

```
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

Throughputs for "add", "sub", "mul", "div", "rem", "land", "lor", "lxor", "lsl", "lsr", "asr" each running 5 times for at least 10 CPU seconds:
 add: 10.74 WALL (10.73 usr +  0.00 sys = 10.73 CPU) @ 2105491.81/s (n=22593961)
      10.70 WALL (10.69 usr +  0.00 sys = 10.69 CPU) @ 2112712.18/s (n=22593961)
      11.25 WALL (11.23 usr +  0.00 sys = 11.23 CPU) @ 2011612.69/s (n=22593961)
      11.54 WALL (11.54 usr +  0.00 sys = 11.54 CPU) @ 1958311.91/s (n=22593961)
      13.18 WALL (13.16 usr +  0.00 sys = 13.17 CPU) @ 1716186.77/s (n=22593961)
 sub: 10.88 WALL (10.86 usr +  0.00 sys = 10.86 CPU) @ 1788803.69/s (n=19433227)
      10.83 WALL (10.82 usr +  0.00 sys = 10.82 CPU) @ 1795687.72/s (n=19433227)
      10.79 WALL (10.79 usr +  0.00 sys = 10.79 CPU) @ 1801527.86/s (n=19433227)
      10.56 WALL (10.56 usr +  0.00 sys = 10.56 CPU) @ 1840467.79/s (n=19433227)
      11.81 WALL (11.80 usr +  0.00 sys = 11.80 CPU) @ 1647243.52/s (n=19433227)
 mul: 10.12 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 1923682.83/s (n=19465735)
      10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 1859353.58/s (n=18780875)
      10.12 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 1963063.67/s (n=19863723)
      10.01 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 1982929.99/s (n=19848223)
      10.12 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 1988744.86/s (n=20127677)
 div: 10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 1677380.59/s (n=16945048)
      10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 1904713.23/s (n=19246152)
      10.09 WALL (10.09 usr +  0.00 sys = 10.09 CPU) @ 1824035.81/s (n=18405893)
      10.13 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 1930525.33/s (n=19538455)
      10.13 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 1932513.07/s (n=19575194)
 rem: 11.01 WALL (11.01 usr +  0.00 sys = 11.01 CPU) @ 1845839.92/s (n=20317819)
      11.48 WALL (11.47 usr +  0.00 sys = 11.47 CPU) @ 1771106.54/s (n=20317819)
      10.79 WALL (10.79 usr +  0.00 sys = 10.79 CPU) @ 1883330.62/s (n=20317819)
      10.83 WALL (10.82 usr +  0.00 sys = 10.82 CPU) @ 1877359.51/s (n=20317819)
      10.46 WALL (10.46 usr +  0.00 sys = 10.46 CPU) @ 1943237.32/s (n=20317819)
land: 10.14 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 1987888.98/s (n=20146404)
      11.49 WALL (11.44 usr +  0.00 sys = 11.45 CPU) @ 1759700.61/s (n=20146404)
      10.88 WALL (10.86 usr +  0.00 sys = 10.86 CPU) @ 1855771.17/s (n=20146404)
      10.80 WALL (10.79 usr +  0.00 sys = 10.79 CPU) @ 1866746.48/s (n=20146404)
      10.26 WALL (10.26 usr +  0.00 sys = 10.26 CPU) @ 1964447.01/s (n=20146404)
 lor: 10.51 WALL (10.51 usr +  0.00 sys = 10.51 CPU) @ 1855206.20/s (n=19492505)
      11.10 WALL (11.08 usr +  0.01 sys = 11.09 CPU) @ 1758071.00/s (n=19492505)
      11.58 WALL (11.55 usr +  0.00 sys = 11.55 CPU) @ 1687349.11/s (n=19492505)
      11.06 WALL (11.05 usr +  0.00 sys = 11.05 CPU) @ 1764222.86/s (n=19492505)
      10.69 WALL (10.68 usr +  0.00 sys = 10.68 CPU) @ 1824738.21/s (n=19492505)
lxor: 10.46 WALL (10.46 usr +  0.00 sys = 10.46 CPU) @ 2087698.14/s (n=21837761)
      10.51 WALL (10.51 usr +  0.00 sys = 10.51 CPU) @ 2078326.79/s (n=21837761)
      10.28 WALL (10.28 usr +  0.00 sys = 10.28 CPU) @ 2125213.71/s (n=21837761)
      10.32 WALL (10.31 usr +  0.00 sys = 10.32 CPU) @ 2117054.17/s (n=21837761)
      10.33 WALL (10.32 usr +  0.00 sys = 10.32 CPU) @ 2115324.82/s (n=21837761)
 lsl: 10.46 WALL (10.46 usr +  0.00 sys = 10.46 CPU) @ 2118265.79/s (n=22164150)
      10.44 WALL (10.43 usr +  0.00 sys = 10.43 CPU) @ 2124202.52/s (n=22164150)
      10.49 WALL (10.49 usr +  0.00 sys = 10.49 CPU) @ 2112109.53/s (n=22164150)
      10.97 WALL (10.96 usr +  0.00 sys = 10.96 CPU) @ 2021813.99/s (n=22164150)
      10.68 WALL (10.68 usr +  0.00 sys = 10.68 CPU) @ 2076256.87/s (n=22164150)
 lsr: 10.24 WALL (10.24 usr +  0.00 sys = 10.24 CPU) @ 2063093.99/s (n=21125022)
      10.14 WALL (10.14 usr +  0.00 sys = 10.14 CPU) @ 2082974.58/s (n=21125022)
      10.11 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 2115180.56/s (n=21387166)
      10.27 WALL (10.26 usr +  0.00 sys = 10.26 CPU) @ 2058393.02/s (n=21125022)
      10.54 WALL (10.53 usr +  0.00 sys = 10.53 CPU) @ 2005667.51/s (n=21125022)
 asr: 10.56 WALL (10.55 usr +  0.00 sys = 10.55 CPU) @ 2075221.25/s (n=21901441)
      10.47 WALL (10.47 usr +  0.00 sys = 10.47 CPU) @ 2092662.24/s (n=21901441)
      10.35 WALL (10.35 usr +  0.00 sys = 10.35 CPU) @ 2116358.53/s (n=21901441)
      10.34 WALL (10.34 usr +  0.00 sys = 10.34 CPU) @ 2118466.22/s (n=21901441)
      11.16 WALL (11.15 usr +  0.00 sys = 11.15 CPU) @ 1963609.54/s (n=21901441)

Benchmark two parameters
          Rate          sub   lor   div   rem  land   mul   add   lsr   asr   lsl  lxor
 sub 1774746+- 62915/s   -- [-0%] [-4%] [-5%] [-6%]   -9%  -10%  -14%  -14%  -15%  -16%
 lor 1777917+- 55312/s [0%]    -- [-4%] [-5%] [-6%]   -9%  -10%  -14%  -14%  -15%  -16%
 div 1853834+- 91796/s [4%]  [4%]    -- [-1%] [-2%] [-5%] [-6%]  -10%  -11%  -11%  -12%
 rem 1864175+- 53385/s [5%]  [5%]  [1%]    -- [-1%] [-4%] [-6%]  -10%  -10%  -11%  -11%
land 1886911+- 78070/s [6%]  [6%]  [2%]  [1%]    -- [-3%] [-5%]   -9%   -9%  -10%  -10%
 mul 1943555+- 45477/s  10%    9%  [5%]  [4%]  [3%]    -- [-2%]   -6%   -6%   -7%   -8%
 add 1980863+-137290/s  12%   11%  [7%]  [6%]  [5%]  [2%]    -- [-4%] [-4%] [-5%] [-6%]
 lsr 2065062+- 34012/s  16%   16%   11%   11%    9%    6%  [4%]    -- [-0%] [-1%] [-2%]
 asr 2073264+- 54249/s  17%   17%   12%   11%   10%    7%  [5%]  [0%]    -- [-1%] [-1%]
 lsl 2090530+- 36286/s  18%   18%   13%   12%   11%    8%  [6%]  [1%]  [1%]    -- [-1%]
lxor 2104724+- 17367/s  19%   18%   14%   13%   12%    8%  [6%]  [2%]  [2%]  [1%]    --
Latencies for 20000 iterations of "add", "sub", "mul", "div", "rem", "land", "lor", "lxor", "lsl", "lsr", "asr":
 add:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1722207.87/s (n=20000)
      (warning: too few iterations for a reliable count)
 sub:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1188707.28/s (n=20000)
      (warning: too few iterations for a reliable count)
 mul:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 963623.22/s (n=20000)
      (warning: too few iterations for a reliable count)
 div:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1948558.07/s (n=20000)
      (warning: too few iterations for a reliable count)
 rem:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1823819.08/s (n=20000)
      (warning: too few iterations for a reliable count)
land:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1012043.32/s (n=20000)
      (warning: too few iterations for a reliable count)
 lor:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1882175.80/s (n=20000)
      (warning: too few iterations for a reliable count)
lxor:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1147842.06/s (n=20000)
      (warning: too few iterations for a reliable count)
 lsl:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 2029632.64/s (n=20000)
      (warning: too few iterations for a reliable count)
 lsr:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1369300.29/s (n=20000)
      (warning: too few iterations for a reliable count)
 asr:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1561889.89/s (n=20000)
      (warning: too few iterations for a reliable count)

          Rate  mul land lxor  sub  lsr  asr  add  rem  lor  div  lsl
 mul  963623/s   --  -5% -16% -19% -30% -38% -44% -47% -49% -51% -53%
land 1012043/s   5%   -- -12% -15% -26% -35% -41% -45% -46% -48% -50%
lxor 1147842/s  19%  13%   --  -3% -16% -27% -33% -37% -39% -41% -43%
 sub 1188707/s  23%  17%   4%   -- -13% -24% -31% -35% -37% -39% -41%
 lsr 1369300/s  42%  35%  19%  15%   -- -12% -20% -25% -27% -30% -33%
 asr 1561890/s  62%  54%  36%  31%  14%   --  -9% -14% -17% -20% -23%
 add 1722208/s  79%  70%  50%  45%  26%  10%   --  -6%  -8% -12% -15%
 rem 1823819/s  89%  80%  59%  53%  33%  17%   6%   --  -3%  -6% -10%
 lor 1882176/s  95%  86%  64%  58%  37%  21%   9%   3%   --  -3%  -7%
 div 1948558/s 102%  93%  70%  64%  42%  25%  13%   7%   4%   --  -4%
 lsl 2029633/s 111% 101%  77%  71%  48%  30%  18%  11%   8%   4%   --
```

## Recursion

Source `bench_time_recursion.ml`

### Iterpreter

```
Throughputs for "factorial n=1", "fibonacci n=1" each running 5 times for at least 10 CPU seconds:
factorial n=1: 11.56 WALL (11.51 usr +  0.00 sys = 11.51 CPU) @ 585876.36/s (n=6743315)
               11.47 WALL (11.45 usr +  0.00 sys = 11.45 CPU) @ 588769.67/s (n=6743315)
               12.33 WALL (12.32 usr +  0.00 sys = 12.32 CPU) @ 547203.62/s (n=6743315)
               13.59 WALL (13.57 usr +  0.00 sys = 13.57 CPU) @ 496901.86/s (n=6743315)
               14.38 WALL (14.34 usr +  0.00 sys = 14.34 CPU) @ 470365.96/s (n=6743315)
fibonacci n=1: 11.89 WALL (11.89 usr +  0.00 sys = 11.89 CPU) @ 481982.15/s (n=5728874)
               10.82 WALL (10.82 usr +  0.00 sys = 10.82 CPU) @ 529363.89/s (n=5728874)
               10.71 WALL (10.71 usr +  0.00 sys = 10.71 CPU) @ 534889.64/s (n=5728874)
               12.03 WALL (12.01 usr +  0.00 sys = 12.01 CPU) @ 477180.30/s (n=5728874)
               11.40 WALL (11.39 usr +  0.00 sys = 11.39 CPU) @ 502918.95/s (n=5728874)

Benchmark recursion
                  Rate        fibonacci n=1 factorial n=1
fibonacci n=1 505267+-22460/s            --         [-6%]
factorial n=1 537823+-44998/s          [6%]            --
Latencies for 20000 iterations of "factorial n=1", "fibonacci n=1":
factorial n=1:  0.04 WALL ( 0.04 usr +  0.00 sys =  0.04 CPU) @ 449478.60/s (n=20000)
               (warning: too few iterations for a reliable count)
fibonacci n=1:  0.05 WALL ( 0.05 usr +  0.00 sys =  0.05 CPU) @ 409643.00/s (n=20000)
               (warning: too few iterations for a reliable count)

                  Rate fibonacci n=1 factorial n=1
fibonacci n=1 409643/s            --           -9%
factorial n=1 449479/s           10%            --
```