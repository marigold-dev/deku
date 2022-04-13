# Benchmark of DEKU by using the library ocaml-benchmark

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

Benchmarks of tezos rpc, ledger, validators, incremental patricia tree, and tezos internal operations.

## Run

- esy:
    - Build: `esy x dune build`
    - Executue: `esy x dune exec ~/deku/_build/default/benchmarks/time_bench/bench_deku/bench_time_deku.exe` 
    or using alias:
    TODO

- script: TODO

## Understanding the results

[Benchmark documentation](https://chris00.github.io/ocaml-benchmark/doc/benchmark/Benchmark/index.html)

TODO

## Ledger

Source `bench_time_ledger.ml`

```
Throughputs for "make_ticket", "make_address", "make_tezos_address" each running 5 times for at least 10 CPU seconds:
       make_ticket: 10.13 WALL (10.10 usr +  0.01 sys = 10.11 CPU) @ 592216.72/s (n=5987603)
                    10.12 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 650944.51/s (n=6583065)
                    10.01 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 628682.38/s (n=6293193)
                    10.08 WALL (10.06 usr +  0.01 sys = 10.07 CPU) @ 598467.79/s (n=6025193)
                    10.13 WALL (10.06 usr +  0.01 sys = 10.07 CPU) @ 592198.42/s (n=5961668)
      make_address: 10.81 WALL (10.78 usr +  0.00 sys = 10.78 CPU) @ 37684.35/s (n=406233)
                    12.54 WALL (12.50 usr +  0.00 sys = 12.50 CPU) @ 32487.37/s (n=406233)
                    12.29 WALL (12.27 usr +  0.00 sys = 12.27 CPU) @ 33103.62/s (n=406233)
                    12.79 WALL (12.79 usr +  0.00 sys = 12.79 CPU) @ 31756.75/s (n=406233)
                    11.66 WALL (11.65 usr +  0.00 sys = 11.66 CPU) @ 34854.03/s (n=406233)
make_tezos_address: 12.11 WALL (12.11 usr +  0.00 sys = 12.11 CPU) @ 32668.50/s (n=395500)
                    12.21 WALL (12.20 usr +  0.00 sys = 12.20 CPU) @ 32408.03/s (n=395500)
                    11.14 WALL (11.14 usr +  0.00 sys = 11.14 CPU) @ 35513.01/s (n=395500)
                    12.75 WALL (12.75 usr +  0.00 sys = 12.75 CPU) @ 31024.09/s (n=395500)
                    11.27 WALL (11.26 usr +  0.00 sys = 11.26 CPU) @ 35120.82/s (n=395500)

Benchmark create
                       Rate        make_tezos_address make_address   make_ticket
make_tezos_address  33347+- 1622/s                 --        [-2%]          -95%
      make_address  33977+- 2012/s               [2%]           --          -94%
       make_ticket 612502+-22321/s              1737%        1703%            --
Latencies for 20000 iterations of "make_ticket", "make_address", "make_tezos_address":
       make_ticket:  0.04 WALL ( 0.04 usr +  0.00 sys =  0.04 CPU) @ 510503.61/s (n=20000)
                    (warning: too few iterations for a reliable count)
      make_address:  0.62 WALL ( 0.62 usr +  0.00 sys =  0.62 CPU) @ 32483.72/s (n=20000)
make_tezos_address:  0.58 WALL ( 0.58 usr +  0.00 sys =  0.58 CPU) @ 34709.57/s (n=20000)

                       Rate     make_address make_tezos_address      make_ticket
      make_address  32484/s               --                -6%             -94%
make_tezos_address  34710/s               7%                 --             -93%
       make_ticket 510504/s            1472%              1371%               --
Throughputs for "deposit", "4_deposits" each running 5 times for at least 10 CPU seconds:
   deposit: 10.08 WALL (10.07 usr +  0.00 sys = 10.07 CPU) @ 33180.79/s (n=334272)
            10.37 WALL (10.37 usr +  0.00 sys = 10.37 CPU) @ 31849.07/s (n=330176)
            10.12 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 33915.34/s (n=342973)
            10.73 WALL (10.71 usr +  0.00 sys = 10.71 CPU) @ 30815.37/s (n=330176)
            11.12 WALL (11.05 usr +  0.00 sys = 11.05 CPU) @ 29885.62/s (n=330176)
4_deposits: 10.65 WALL (10.63 usr +  0.00 sys = 10.63 CPU) @ 16187.03/s (n=172138)
            11.58 WALL (11.55 usr +  0.00 sys = 11.55 CPU) @ 14901.40/s (n=172138)
            11.73 WALL (11.73 usr +  0.00 sys = 11.73 CPU) @ 14673.14/s (n=172138)
            11.00 WALL (11.00 usr +  0.00 sys = 11.00 CPU) @ 15655.43/s (n=172138)
            13.20 WALL (13.19 usr +  0.00 sys = 13.19 CPU) @ 13051.55/s (n=172138)

Deposit
              Rate       4_deposits    deposit
4_deposits 14894+-1014/s         --       -53%
   deposit 31929+-1405/s       114%         --
Latencies for 20000 iterations of "deposit", "4_deposits":
   deposit:  0.70 WALL ( 0.70 usr +  0.00 sys =  0.70 CPU) @ 28415.27/s (n=20000)
4_deposits:  1.37 WALL ( 1.37 usr +  0.00 sys =  1.37 CPU) @ 14596.91/s (n=20000)

              Rate 4_deposits    deposit
4_deposits 14597/s         --       -49%
   deposit 28415/s        95%         --
Throughputs for "balance_deposit", "balance_4_deposits" each running 5 times for at least 10 CPU seconds:
   balance_deposit: 11.95 WALL (11.95 usr +  0.00 sys = 11.95 CPU) @ 28843.15/s (n=344619)
                    12.02 WALL (12.02 usr +  0.00 sys = 12.02 CPU) @ 28668.54/s (n=344619)
                    11.40 WALL (11.39 usr +  0.00 sys = 11.39 CPU) @ 30245.89/s (n=344619)
                    11.44 WALL (11.43 usr +  0.00 sys = 11.43 CPU) @ 30147.17/s (n=344619)
                    11.65 WALL (11.65 usr +  0.00 sys = 11.65 CPU) @ 29582.46/s (n=344619)
balance_4_deposits: 12.82 WALL (12.74 usr +  0.00 sys = 12.74 CPU) @ 13044.12/s (n=166202)
                    10.76 WALL (10.75 usr +  0.00 sys = 10.75 CPU) @ 15465.46/s (n=166202)
                    12.25 WALL (12.23 usr +  0.00 sys = 12.23 CPU) @ 13586.71/s (n=166202)
                    12.36 WALL (12.35 usr +  0.00 sys = 12.35 CPU) @ 13454.74/s (n=166202)
                    12.07 WALL (12.03 usr +  0.00 sys = 12.03 CPU) @ 13812.55/s (n=166202)

Balance
                      Rate      balance_4_deposits    balance_deposit
balance_4_deposits 13873+-793/s                 --               -53%
   balance_deposit 29497+-616/s               113%                 --
Latencies for 20000 iterations of "balance_deposit", "balance_4_deposits":
   balance_deposit:  0.67 WALL ( 0.67 usr +  0.00 sys =  0.67 CPU) @ 30057.83/s (n=20000)
balance_4_deposits:  1.35 WALL ( 1.35 usr +  0.00 sys =  1.35 CPU) @ 14824.92/s (n=20000)

                      Rate balance_4_deposits    balance_deposit
balance_4_deposits 14825/s                 --               -51%
   balance_deposit 30058/s               103%                 --
Throughputs for "transfer 1", "transfer 4" each running 5 times for at least 10 CPU seconds:
transfer 1: 11.09 WALL (11.09 usr +  0.00 sys = 11.09 CPU) @ 16576.61/s (n=183771)
            11.96 WALL (11.95 usr +  0.00 sys = 11.95 CPU) @ 15377.33/s (n=183771)
            10.99 WALL (10.98 usr +  0.00 sys = 10.98 CPU) @ 16733.67/s (n=183771)
            12.09 WALL (12.08 usr +  0.00 sys = 12.08 CPU) @ 15218.78/s (n=183771)
            13.71 WALL (13.63 usr +  0.00 sys = 13.63 CPU) @ 13479.94/s (n=183771)
transfer 4: 10.83 WALL (10.82 usr +  0.00 sys = 10.83 CPU) @ 15726.01/s (n=170274)
            11.12 WALL (11.12 usr +  0.00 sys = 11.12 CPU) @ 15317.87/s (n=170274)
            10.67 WALL (10.66 usr +  0.00 sys = 10.66 CPU) @ 15971.73/s (n=170274)
            11.07 WALL (11.07 usr +  0.00 sys = 11.07 CPU) @ 15380.96/s (n=170274)
            12.29 WALL (12.29 usr +  0.00 sys = 12.29 CPU) @ 13853.81/s (n=170274)

Transfer
              Rate       transfer 4 transfer 1
transfer 4 15250+- 701/s         --      [-1%]
transfer 1 15477+-1112/s       [1%]         --
Latencies for 20000 iterations of "transfer 1", "transfer 4":
transfer 1:  1.37 WALL ( 1.36 usr +  0.00 sys =  1.36 CPU) @ 14678.18/s (n=20000)
transfer 4:  1.34 WALL ( 1.34 usr +  0.00 sys =  1.34 CPU) @ 14919.62/s (n=20000)

              Rate transfer 1 transfer 4
transfer 1 14678/s         --        -2%
transfer 4 14920/s         2%         --
Throughputs for "withdraw 1", "withdraw 4" each running 5 times for at least 10 CPU seconds:
withdraw 1: 10.42 WALL (10.42 usr +  0.00 sys = 10.42 CPU) @ 10462.85/s (n=109003)
            10.81 WALL (10.81 usr +  0.00 sys = 10.81 CPU) @ 10085.41/s (n=109003)
            11.14 WALL (11.14 usr +  0.00 sys = 11.14 CPU) @ 9785.13/s (n=109003)
            10.90 WALL (10.88 usr +  0.00 sys = 10.88 CPU) @ 10014.55/s (n=109003)
            11.12 WALL (11.11 usr +  0.00 sys = 11.11 CPU) @ 9814.25/s (n=109003)
withdraw 4: 10.75 WALL (10.75 usr +  0.00 sys = 10.75 CPU) @ 7744.30/s (n=83244)
            10.68 WALL (10.67 usr +  0.00 sys = 10.67 CPU) @ 7800.26/s (n=83244)
            11.15 WALL (11.15 usr +  0.00 sys = 11.15 CPU) @ 7466.48/s (n=83244)
            10.53 WALL (10.52 usr +  0.00 sys = 10.53 CPU) @ 7907.96/s (n=83244)
            11.30 WALL (11.30 usr +  0.00 sys = 11.30 CPU) @ 7369.17/s (n=83244)

Withdraw
              Rate      withdraw 4 withdraw 1
withdraw 4  7658+-195/s         --       -24%
withdraw 1 10032+-232/s        31%         --
Latencies for 20000 iterations of "withdraw 1", "withdraw 4":
withdraw 1:  2.12 WALL ( 2.12 usr +  0.00 sys =  2.12 CPU) @ 9412.37/s (n=20000)
withdraw 4:  3.01 WALL ( 3.01 usr +  0.00 sys =  3.01 CPU) @ 6652.29/s (n=20000)

             Rate withdraw 4 withdraw 1
withdraw 4 6652/s         --       -29%
withdraw 1 9412/s        41%         --
```