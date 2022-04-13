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

## Patricia

Source `bench_time_patricia.ml`

```
Throughputs for "add_find", "hash_tree", "hash_values" each running 5 times for at least 10 CPU seconds:
   add_find: 10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 4025046.53/s (n=40650253)
             10.04 WALL (10.03 usr +  0.00 sys = 10.03 CPU) @ 3866036.74/s (n=38768052)
             12.10 WALL (12.10 usr +  0.00 sys = 12.10 CPU) @ 3077011.56/s (n=37236800)
             10.88 WALL (10.87 usr +  0.00 sys = 10.87 CPU) @ 3426349.92/s (n=37236800)
             10.19 WALL (10.19 usr +  0.00 sys = 10.19 CPU) @ 3655894.24/s (n=37236800)
  hash_tree: 10.97 WALL (10.97 usr +  0.00 sys = 10.97 CPU) @ 356208.55/s (n=3906417)
             10.23 WALL (10.23 usr +  0.00 sys = 10.23 CPU) @ 381958.37/s (n=3906417)
             10.59 WALL (10.59 usr +  0.00 sys = 10.59 CPU) @ 368960.20/s (n=3906417)
             10.94 WALL (10.94 usr +  0.00 sys = 10.94 CPU) @ 356990.13/s (n=3906417)
             10.27 WALL (10.27 usr +  0.00 sys = 10.27 CPU) @ 380333.41/s (n=3906417)
hash_values: 11.35 WALL (11.34 usr +  0.00 sys = 11.35 CPU) @ 282062.08/s (n=3200161)
             10.77 WALL (10.77 usr +  0.00 sys = 10.77 CPU) @ 297216.12/s (n=3200161)
             10.69 WALL (10.69 usr +  0.00 sys = 10.69 CPU) @ 299481.60/s (n=3200161)
             12.18 WALL (12.16 usr +  0.00 sys = 12.16 CPU) @ 263086.67/s (n=3200161)
             10.89 WALL (10.89 usr +  0.00 sys = 10.89 CPU) @ 293973.41/s (n=3200161)

Benchmark Patricia
                 Rate         hash_values   hash_tree    add_find
hash_values  287164+- 12780/s          --        -22%        -92%
  hash_tree  368890+- 10443/s         28%          --        -90%
   add_find 3610068+-317285/s       1157%        879%          --
Latencies for 20000 iterations of "add_find", "hash_tree", "hash_values":
   add_find:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 3162555.34/s (n=20000)
             (warning: too few iterations for a reliable count)
  hash_tree:  0.07 WALL ( 0.07 usr +  0.00 sys =  0.07 CPU) @ 269687.16/s (n=20000)
             (warning: too few iterations for a reliable count)
hash_values:  0.09 WALL ( 0.09 usr +  0.00 sys =  0.09 CPU) @ 224159.96/s (n=20000)
             (warning: too few iterations for a reliable count)

                 Rate hash_values   hash_tree    add_find
hash_values  224160/s          --        -17%        -93%
  hash_tree  269687/s         20%          --        -91%
   add_find 3162555/s       1311%       1073%          --
```


## Tezos_interop

Source `bench_time_tezos_interop.ml`

```
Throughputs for "keys: to_string", "keys: of_string", "keys: hash", "secret keys: to_string", "secret keys: of_string" each running 5 times for at least 10 CPU seconds:
       keys: to_string: 10.10 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 19194.22/s (n=193880)
                        10.18 WALL (10.18 usr +  0.00 sys = 10.18 CPU) @ 19450.80/s (n=197976)
                        11.94 WALL (11.93 usr +  0.00 sys = 11.93 CPU) @ 16249.21/s (n=193880)
                        11.78 WALL (11.77 usr +  0.00 sys = 11.77 CPU) @ 16473.32/s (n=193880)
                        13.18 WALL (13.17 usr +  0.00 sys = 13.17 CPU) @ 14718.20/s (n=193880)
       keys: of_string: 13.37 WALL (13.37 usr +  0.00 sys = 13.37 CPU) @ 1167.45/s (n=15611)
                        12.51 WALL (12.50 usr +  0.00 sys = 12.50 CPU) @ 1248.53/s (n=15611)
                        13.06 WALL (12.97 usr +  0.02 sys = 12.98 CPU) @ 1202.38/s (n=15611)
                        12.84 WALL (12.80 usr +  0.00 sys = 12.80 CPU) @ 1219.56/s (n=15611)
                        12.89 WALL (12.89 usr +  0.00 sys = 12.89 CPU) @ 1211.05/s (n=15611)
            keys: hash: 11.72 WALL (11.72 usr +  0.00 sys = 11.72 CPU) @ 23526.71/s (n=275692)
                        13.60 WALL (13.57 usr +  0.00 sys = 13.57 CPU) @ 20312.95/s (n=275692)
                        14.13 WALL (14.11 usr +  0.00 sys = 14.11 CPU) @ 19535.95/s (n=275692)
                        13.63 WALL (13.63 usr +  0.00 sys = 13.63 CPU) @ 20229.24/s (n=275692)
                        13.68 WALL (13.68 usr +  0.00 sys = 13.68 CPU) @ 20147.80/s (n=275692)
secret keys: to_string: 10.63 WALL (10.63 usr +  0.00 sys = 10.63 CPU) @ 50090.07/s (n=532384)
                        11.85 WALL (11.84 usr +  0.00 sys = 11.84 CPU) @ 44957.13/s (n=532384)
                        10.95 WALL (10.95 usr +  0.00 sys = 10.95 CPU) @ 48640.64/s (n=532384)
                        10.71 WALL (10.71 usr +  0.00 sys = 10.71 CPU) @ 49709.33/s (n=532384)
                        11.41 WALL (11.41 usr +  0.00 sys = 11.41 CPU) @ 46678.29/s (n=532384)
secret keys: of_string: 10.05 WALL (10.05 usr +  0.00 sys = 10.05 CPU) @ 29762.48/s (n=299059)
                        10.09 WALL (10.07 usr +  0.00 sys = 10.07 CPU) @ 29416.00/s (n=296238)
                        10.08 WALL (10.01 usr +  0.00 sys = 10.02 CPU) @ 28011.71/s (n=280631)
                        10.13 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 28993.17/s (n=293565)
                        10.03 WALL (10.03 usr +  0.00 sys = 10.03 CPU) @ 27933.24/s (n=280248)

Benchmark keys
                          Rate       keys: of_string keys: to_string keys: hash secret keys: of_string secret keys: to_string
       keys: of_string  1210+-  25/s              --            -93%       -94%                   -96%                   -97%
       keys: to_string 17217+-1733/s           1323%              --       -17%                   -40%                   -64%
            keys: hash 20751+-1344/s           1615%             21%         --                   -28%                   -57%
secret keys: of_string 28823+- 700/s           2282%             67%        39%                     --                   -40%
secret keys: to_string 48015+-1837/s           3869%            179%       131%                    67%                     --
Latencies for 20000 iterations of "keys: to_string", "keys: of_string", "keys: hash", "secret keys: to_string", "secret keys: of_string":
       keys: to_string:  1.33 WALL ( 1.33 usr +  0.00 sys =  1.33 CPU) @ 15005.63/s (n=20000)
       keys: of_string: 16.62 WALL (16.62 usr +  0.00 sys = 16.62 CPU) @ 1203.51/s (n=20000)
            keys: hash:  0.85 WALL ( 0.85 usr +  0.00 sys =  0.85 CPU) @ 23465.28/s (n=20000)
secret keys: to_string:  0.36 WALL ( 0.36 usr +  0.00 sys =  0.36 CPU) @ 55177.74/s (n=20000)
                        (warning: too few iterations for a reliable count)
secret keys: of_string:  0.60 WALL ( 0.60 usr +  0.00 sys =  0.60 CPU) @ 33454.49/s (n=20000)

                          Rate keys: of_string keys: to_string keys: hash secret keys: of_string secret keys: to_string
       keys: of_string  1204/s              --            -92%       -95%                   -96%                   -98%
       keys: to_string 15006/s           1147%              --       -36%                   -55%                   -73%
            keys: hash 23465/s           1850%             56%         --                   -30%                   -57%
secret keys: of_string 33454/s           2680%            123%        43%                     --                   -39%
secret keys: to_string 55178/s           4485%            268%       135%                    65%                     --
Throughputs for "verify signature" running 5 times for at least 10 CPU seconds:
verify signature: 12.33 WALL (12.28 usr +  0.01 sys = 12.29 CPU) @ 98.85/s (n=1215)
                  12.56 WALL (12.53 usr +  0.00 sys = 12.53 CPU) @ 96.96/s (n=1215)
                  12.62 WALL (12.57 usr +  0.00 sys = 12.57 CPU) @ 96.64/s (n=1215)
                  12.78 WALL (12.69 usr +  0.01 sys = 12.70 CPU) @ 95.68/s (n=1215)
                  11.57 WALL (11.52 usr +  0.00 sys = 11.53 CPU) @ 105.40/s (n=1215)

Benchmark verify signature
                   Rate      verify signature
verify signature 98.7+-3.3/s               --
Latencies for 20000 iterations of "verify signature":
verify signature: 182.12 WALL (181.81 usr +  0.03 sys = 181.84 CPU) @ 109.99/s (n=20000)

                  Rate verify signature
verify signature 110/s               --
Throughputs for "contract_hash: to_string", "contract_hash: of_string" each running 5 times for at least 10 CPU seconds:
contract_hash: to_string: 10.71 WALL (10.70 usr +  0.00 sys = 10.70 CPU) @ 280191.90/s (n=2997169)
                          11.29 WALL (11.27 usr +  0.00 sys = 11.27 CPU) @ 265830.50/s (n=2997169)
                          11.08 WALL (11.07 usr +  0.00 sys = 11.07 CPU) @ 270652.84/s (n=2997169)
                          11.35 WALL (11.35 usr +  0.00 sys = 11.35 CPU) @ 264045.61/s (n=2997169)
                          11.23 WALL (11.23 usr +  0.00 sys = 11.23 CPU) @ 266905.13/s (n=2997169)
contract_hash: of_string: 10.22 WALL (10.22 usr +  0.00 sys = 10.22 CPU) @ 292453.51/s (n=2987472)
                          10.39 WALL (10.39 usr +  0.00 sys = 10.39 CPU) @ 287562.35/s (n=2987472)
                          10.29 WALL (10.27 usr +  0.00 sys = 10.27 CPU) @ 290966.92/s (n=2987472)
                          10.19 WALL (10.12 usr +  0.01 sys = 10.13 CPU) @ 305586.62/s (n=3096284)
                          10.68 WALL (10.67 usr +  0.00 sys = 10.67 CPU) @ 280008.21/s (n=2987472)

Benchmark contract hash
                             Rate       contract_hash: to_string contract_hash: of_string
contract_hash: to_string 269525+-5467/s                       --                      -7%
contract_hash: of_string 291316+-7914/s                       8%                       --
Latencies for 20000 iterations of "contract_hash: to_string", "contract_hash: of_string":
contract_hash: to_string:  0.10 WALL ( 0.10 usr +  0.00 sys =  0.10 CPU) @ 205634.38/s (n=20000)
                          (warning: too few iterations for a reliable count)
contract_hash: of_string:  0.09 WALL ( 0.09 usr +  0.00 sys =  0.09 CPU) @ 215626.45/s (n=20000)
                          (warning: too few iterations for a reliable count)

                             Rate contract_hash: to_string contract_hash: of_string
contract_hash: to_string 205634/s                       --                      -5%
contract_hash: of_string 215626/s                       5%                       --
Throughputs for "address: to_string", "address: of_string" each running 5 times for at least 10 CPU seconds:
address: to_string: 11.76 WALL (11.74 usr +  0.00 sys = 11.75 CPU) @ 48503.10/s (n=569685)
                    11.17 WALL (11.17 usr +  0.00 sys = 11.17 CPU) @ 51019.99/s (n=569685)
                    10.39 WALL (10.39 usr +  0.00 sys = 10.39 CPU) @ 54837.59/s (n=569685)
                    10.18 WALL (10.18 usr +  0.00 sys = 10.18 CPU) @ 55954.62/s (n=569685)
                    10.58 WALL (10.56 usr +  0.00 sys = 10.56 CPU) @ 53956.10/s (n=569685)
address: of_string: 10.49 WALL (10.43 usr +  0.01 sys = 10.44 CPU) @ 22331.53/s (n=233178)
                    10.03 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 23785.47/s (n=238184)
                    10.02 WALL (10.00 usr +  0.00 sys = 10.01 CPU) @ 24368.53/s (n=243889)
                    10.14 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 25153.83/s (n=254913)
                    10.06 WALL (10.05 usr +  0.00 sys = 10.05 CPU) @ 25606.25/s (n=257453)

Benchmark address
                      Rate       address: of_string address: to_string
address: of_string 24249+-1089/s                 --               -54%
address: to_string 52854+-2586/s               118%                 --
Latencies for 20000 iterations of "address: to_string", "address: of_string":
address: to_string:  0.35 WALL ( 0.35 usr +  0.00 sys =  0.35 CPU) @ 56991.59/s (n=20000)
                    (warning: too few iterations for a reliable count)
address: of_string:  0.80 WALL ( 0.80 usr +  0.00 sys =  0.80 CPU) @ 25105.95/s (n=20000)

                      Rate address: of_string address: to_string
address: of_string 25106/s                 --               -56%
address: to_string 56992/s               127%                 --
Throughputs for "ticket: to_string", "ticket: of_string" each running 5 times for at least 10 CPU seconds:
ticket: to_string: 10.09 WALL (10.09 usr +  0.00 sys = 10.09 CPU) @ 108996.95/s (n=1099382)
                   10.01 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 112482.97/s (n=1125475)
                   10.15 WALL (10.14 usr +  0.00 sys = 10.14 CPU) @ 111748.54/s (n=1133035)
                   10.92 WALL (10.92 usr +  0.00 sys = 10.92 CPU) @ 100712.55/s (n=1099382)
                   11.14 WALL (11.13 usr +  0.00 sys = 11.13 CPU) @ 98803.61/s (n=1099382)
ticket: of_string: 10.84 WALL (10.81 usr +  0.01 sys = 10.82 CPU) @ 62680.08/s (n=678356)
                   11.16 WALL (11.15 usr +  0.00 sys = 11.15 CPU) @ 60853.87/s (n=678356)
                   10.27 WALL (10.26 usr +  0.00 sys = 10.26 CPU) @ 66107.32/s (n=678356)
                   11.26 WALL (11.26 usr +  0.00 sys = 11.26 CPU) @ 60236.64/s (n=678356)
                   11.03 WALL (11.03 usr +  0.00 sys = 11.03 CPU) @ 61510.06/s (n=678356)

Benchmark ticket
                      Rate       ticket: of_string ticket: to_string
ticket: of_string  62278+-1975/s                --              -42%
ticket: to_string 106549+-5412/s               71%                --
Latencies for 20000 iterations of "ticket: to_string", "ticket: of_string":
ticket: to_string:  0.20 WALL ( 0.20 usr +  0.00 sys =  0.20 CPU) @ 101786.87/s (n=20000)
                   (warning: too few iterations for a reliable count)
ticket: of_string:  0.29 WALL ( 0.29 usr +  0.00 sys =  0.29 CPU) @ 67814.08/s (n=20000)
                   (warning: too few iterations for a reliable count)

                      Rate ticket: of_string ticket: to_string
ticket: of_string  67814/s                --              -33%
ticket: to_string 101787/s               50%                --
Throughputs for "operation hash" running 5 times for at least 10 CPU seconds:
operation hash: 10.72 WALL (10.71 usr +  0.00 sys = 10.71 CPU) @ 118628.64/s (n=1270915)
                11.02 WALL (11.02 usr +  0.00 sys = 11.02 CPU) @ 115305.19/s (n=1270915)
                10.83 WALL (10.83 usr +  0.00 sys = 10.83 CPU) @ 117401.07/s (n=1270915)
                11.67 WALL (11.67 usr +  0.00 sys = 11.67 CPU) @ 108932.94/s (n=1270915)
                13.84 WALL (13.82 usr +  0.00 sys = 13.82 CPU) @ 91964.21/s (n=1270915)

Benchmark operation hash
                   Rate       operation hash
operation hash 110446+-9335/s             --
Latencies for 20000 iterations of "operation hash":
operation hash:  0.20 WALL ( 0.20 usr +  0.00 sys =  0.20 CPU) @ 98772.75/s (n=20000)
                (warning: too few iterations for a reliable count)

                  Rate operation hash
operation hash 98773/s             --
Throughputs for "forge transaction taquito", "forge transaction bytes" each running 5 times for at least 10 CPU seconds:
forge transaction taquito: 11.54 WALL (11.52 usr +  0.00 sys = 11.52 CPU) @ 12713.69/s (n=146495)
                           12.08 WALL (12.08 usr +  0.00 sys = 12.08 CPU) @ 12131.05/s (n=146495)
                           13.09 WALL (13.07 usr +  0.00 sys = 13.07 CPU) @ 11205.54/s (n=146495)
                           13.36 WALL (13.32 usr +  0.00 sys = 13.33 CPU) @ 10991.61/s (n=146495)
                           12.52 WALL (12.50 usr +  0.00 sys = 12.50 CPU) @ 11717.30/s (n=146495)
  forge transaction bytes: 12.35 WALL (12.33 usr +  0.00 sys = 12.33 CPU) @ 9915.47/s (n=122243)
                           12.71 WALL (12.70 usr +  0.00 sys = 12.70 CPU) @ 9628.30/s (n=122243)
                           13.03 WALL (13.01 usr +  0.00 sys = 13.01 CPU) @ 9393.66/s (n=122243)
                           12.61 WALL (12.57 usr +  0.01 sys = 12.57 CPU) @ 9721.75/s (n=122243)
                           11.96 WALL (11.89 usr +  0.00 sys = 11.89 CPU) @ 10280.64/s (n=122243)

Benchmark forge transaction 
                             Rate      forge transaction bytes forge transaction taquito
  forge transaction bytes  9788+-283/s                      --                      -17%
forge transaction taquito 11752+-593/s                     20%                        --
Latencies for 20000 iterations of "forge transaction taquito", "forge transaction bytes":
forge transaction taquito:  1.53 WALL ( 1.53 usr +  0.00 sys =  1.53 CPU) @ 13080.22/s (n=20000)
  forge transaction bytes:  1.85 WALL ( 1.85 usr +  0.00 sys =  1.85 CPU) @ 10789.52/s (n=20000)

                             Rate forge transaction bytes forge transaction taquito
  forge transaction bytes 10790/s                      --                      -18%
forge transaction taquito 13080/s                     21%                        --
Throughputs for "pack: int 1", "pack: int -1", "pack: bytes 0x", "pack: bytes 050001", "pack: pair (1, 0x)", "pack: (1, (0xAA, -1))", "pack: list empty", "pack: list int 1", "pack: list pair", "pack: key", "pack: key hash", "pack: address implicit", "pack: address originated", "pack: to_bytes int", "pack: to_bytes bytes", "pack: to_bytes pair", "pack: to_bytes list", "pack: to_bytes key", "pack: to_bytes key_hash", "pack: to_bytes address_implicit", "pack: to_bytes address_originated" each running 5 times for at least 10 CPU seconds:
                      pack: int 1: 11.70 WALL (11.68 usr +  0.00 sys = 11.69 CPU) @ 411526087.97/s (n=4809985641)
                                   13.65 WALL (13.62 usr +  0.00 sys = 13.62 CPU) @ 353170510.79/s (n=4809985641)
                                   12.08 WALL (11.89 usr +  0.02 sys = 11.91 CPU) @ 403768374.01/s (n=4809985641)
                                   14.04 WALL (14.01 usr +  0.00 sys = 14.02 CPU) @ 343109973.85/s (n=4809985641)
                                   11.50 WALL (11.52 usr +  0.00 sys = 11.52 CPU) @ 417431581.89/s (n=4809985641)
                     pack: int -1: 10.57 WALL (10.57 usr +  0.00 sys = 10.57 CPU) @ 420567557.28/s (n=4447167567)
                                   10.14 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 400866914.97/s (n=4061804861)
                                   11.00 WALL (10.94 usr +  0.00 sys = 10.94 CPU) @ 337582581.88/s (n=3693847178)
                                   10.09 WALL (10.09 usr +  0.01 sys = 10.10 CPU) @ 389396803.20/s (n=3933891718)
                                   10.18 WALL (10.17 usr +  0.00 sys = 10.18 CPU) @ 385655126.59/s (n=3924119201)
                   pack: bytes 0x: 10.14 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 29842429.89/s (n=302342789)
                                   10.06 WALL (10.05 usr +  0.01 sys = 10.06 CPU) @ 33612692.33/s (n=338013234)
                                   10.16 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 32370466.16/s (n=327407325)
                                   10.13 WALL (10.05 usr +  0.00 sys = 10.05 CPU) @ 34793795.91/s (n=349611819)
                                   10.13 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 33190009.14/s (n=335175016)
               pack: bytes 050001: 11.67 WALL (11.64 usr +  0.00 sys = 11.65 CPU) @ 11181271.21/s (n=130217118)
                                   10.17 WALL (10.15 usr +  0.00 sys = 10.16 CPU) @ 13536985.47/s (n=137520611)
                                   10.24 WALL (10.22 usr +  0.00 sys = 10.22 CPU) @ 12736808.50/s (n=130217118)
                                   10.38 WALL (10.36 usr +  0.00 sys = 10.37 CPU) @ 12558679.56/s (n=130217118)
                                   11.08 WALL (11.07 usr +  0.00 sys = 11.07 CPU) @ 11762780.25/s (n=130217118)
               pack: pair (1, 0x): 12.40 WALL (12.35 usr +  0.01 sys = 12.36 CPU) @ 17225338.67/s (n=212930645)
                                   10.67 WALL (10.66 usr +  0.00 sys = 10.66 CPU) @ 19971848.81/s (n=212930645)
                                   12.48 WALL (12.45 usr +  0.00 sys = 12.45 CPU) @ 17103544.45/s (n=212930645)
                                   15.45 WALL (15.45 usr +  0.00 sys = 15.45 CPU) @ 13783585.29/s (n=212930645)
                                   12.23 WALL (12.21 usr +  0.00 sys = 12.21 CPU) @ 17441245.63/s (n=212930645)
            pack: (1, (0xAA, -1)): 10.73 WALL (10.70 usr +  0.00 sys = 10.70 CPU) @ 10970104.83/s (n=117331568)
                                   10.23 WALL (10.22 usr +  0.00 sys = 10.22 CPU) @ 11484379.87/s (n=117331568)
                                   10.07 WALL (10.06 usr +  0.00 sys = 10.06 CPU) @ 12341350.40/s (n=124146790)
                                   10.09 WALL (10.08 usr +  0.00 sys = 10.08 CPU) @ 12271824.57/s (n=123652107)
                                   10.01 WALL ( 9.99 usr +  0.01 sys = 10.00 CPU) @ 12555752.34/s (n=125566212)
                 pack: list empty: 10.12 WALL (10.10 usr +  0.00 sys = 10.11 CPU) @ 386880407.86/s (n=3909796766)
                                   10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 375500523.78/s (n=3793174115)
                                   11.29 WALL (11.28 usr +  0.00 sys = 11.28 CPU) @ 330216588.60/s (n=3726065251)
                                   10.23 WALL (10.23 usr +  0.00 sys = 10.23 CPU) @ 364327154.09/s (n=3726065251)
                                   10.17 WALL (10.16 usr +  0.00 sys = 10.16 CPU) @ 380160766.50/s (n=3863174321)
                 pack: list int 1: 10.03 WALL (10.03 usr +  0.00 sys = 10.03 CPU) @ 209329545.98/s (n=2099768348)
                                   10.03 WALL (10.02 usr +  0.00 sys = 10.02 CPU) @ 193361243.37/s (n=1937529159)
                                   10.06 WALL (10.06 usr +  0.00 sys = 10.06 CPU) @ 198933417.69/s (n=2001894435)
                                   11.62 WALL (11.50 usr +  0.01 sys = 11.51 CPU) @ 137072831.25/s (n=1577343674)
                                   10.50 WALL (10.50 usr +  0.00 sys = 10.50 CPU) @ 202115750.99/s (n=2121835812)
                  pack: list pair: 10.48 WALL (10.46 usr +  0.01 sys = 10.47 CPU) @ 15781635.42/s (n=165212102)
                                   12.04 WALL (12.03 usr +  0.00 sys = 12.03 CPU) @ 13733554.15/s (n=165212102)
                                   11.60 WALL (11.59 usr +  0.00 sys = 11.59 CPU) @ 14260563.01/s (n=165212102)
                                   10.63 WALL (10.60 usr +  0.00 sys = 10.60 CPU) @ 15581619.83/s (n=165212102)
                                   11.81 WALL (11.75 usr +  0.00 sys = 11.75 CPU) @ 14056092.12/s (n=165212102)
                        pack: key: 10.93 WALL (10.88 usr +  0.00 sys = 10.88 CPU) @ 9073473.89/s (n=98706938)
                                   11.65 WALL (11.58 usr +  0.00 sys = 11.58 CPU) @ 8524945.11/s (n=98706938)
                                   12.72 WALL (12.67 usr +  0.00 sys = 12.67 CPU) @ 7788110.28/s (n=98706938)
                                   12.72 WALL (12.69 usr +  0.00 sys = 12.69 CPU) @ 7775954.96/s (n=98706938)
                                   11.46 WALL (11.46 usr +  0.00 sys = 11.46 CPU) @ 8616085.74/s (n=98706938)
                   pack: key hash: 10.52 WALL (10.49 usr +  0.00 sys = 10.49 CPU) @ 2476870.05/s (n=25991338)
                                   10.08 WALL (10.06 usr +  0.00 sys = 10.06 CPU) @ 2583582.19/s (n=25991338)
                                   11.53 WALL (11.48 usr +  0.00 sys = 11.48 CPU) @ 2263977.32/s (n=25991338)
                                   10.55 WALL (10.52 usr +  0.01 sys = 10.53 CPU) @ 2468437.44/s (n=25991338)
                                   10.16 WALL (10.16 usr +  0.00 sys = 10.16 CPU) @ 2688746.78/s (n=27315242)
           pack: address implicit: 10.13 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 2060251.64/s (n=20828901)
                                   10.13 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 2098861.06/s (n=21255124)
                                   10.01 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 2141469.78/s (n=21427476)
                                   10.35 WALL (10.33 usr +  0.00 sys = 10.33 CPU) @ 1860466.32/s (n=19221391)
                                   10.13 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 2084317.39/s (n=21086247)
         pack: address originated: 11.36 WALL (11.34 usr +  0.00 sys = 11.34 CPU) @ 6307097.41/s (n=71521671)
                                   10.69 WALL (10.65 usr +  0.00 sys = 10.66 CPU) @ 6710939.60/s (n=71521671)
                                   11.82 WALL (11.76 usr +  0.00 sys = 11.76 CPU) @ 6080285.70/s (n=71521671)
                                   13.77 WALL (13.68 usr +  0.01 sys = 13.69 CPU) @ 5225453.16/s (n=71521671)
                                   13.32 WALL (13.29 usr +  0.00 sys = 13.29 CPU) @ 5383124.79/s (n=71521671)
               pack: to_bytes int: 13.31 WALL (13.30 usr +  0.00 sys = 13.30 CPU) @ 5979507.58/s (n=79543745)
                                   14.27 WALL (14.24 usr +  0.00 sys = 14.24 CPU) @ 5586931.34/s (n=79543745)
                                   15.72 WALL (15.66 usr +  0.01 sys = 15.67 CPU) @ 5075754.98/s (n=79543745)
                                   14.27 WALL (14.20 usr +  0.00 sys = 14.21 CPU) @ 5598533.85/s (n=79543745)
                                   12.66 WALL (12.65 usr +  0.01 sys = 12.65 CPU) @ 6285680.36/s (n=79543745)
             pack: to_bytes bytes: 10.12 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 4301786.35/s (n=43527608)
                                   10.39 WALL (10.39 usr +  0.00 sys = 10.39 CPU) @ 4190699.74/s (n=43527608)
                                   10.65 WALL (10.60 usr +  0.00 sys = 10.60 CPU) @ 4107269.70/s (n=43527608)
                                   11.36 WALL (11.35 usr +  0.00 sys = 11.35 CPU) @ 3836541.48/s (n=43527608)
                                   10.07 WALL (10.06 usr +  0.00 sys = 10.06 CPU) @ 4324715.02/s (n=43527608)
              pack: to_bytes pair: 10.04 WALL (10.04 usr +  0.00 sys = 10.04 CPU) @ 1850826.63/s (n=18579338)
                                   10.03 WALL (10.02 usr +  0.00 sys = 10.02 CPU) @ 1916473.20/s (n=19212364)
                                   10.13 WALL (10.13 usr +  0.00 sys = 10.13 CPU) @ 1926759.23/s (n=19509389)
                                   10.01 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 1855983.34/s (n=18580819)
                                   10.10 WALL (10.07 usr +  0.01 sys = 10.07 CPU) @ 1683766.43/s (n=16961800)
              pack: to_bytes list: 10.28 WALL (10.26 usr +  0.00 sys = 10.26 CPU) @ 5693730.64/s (n=58419208)
                                   10.20 WALL (10.21 usr +  0.00 sys = 10.21 CPU) @ 5719493.33/s (n=58419208)
                                   10.15 WALL (10.14 usr +  0.00 sys = 10.14 CPU) @ 5760672.85/s (n=58419208)
                                   10.14 WALL (10.13 usr +  0.00 sys = 10.14 CPU) @ 5865297.68/s (n=59467784)
                                   11.68 WALL (11.66 usr +  0.01 sys = 11.67 CPU) @ 5007015.49/s (n=58419208)
               pack: to_bytes key: 10.51 WALL (10.47 usr +  0.02 sys = 10.49 CPU) @ 2913871.97/s (n=30562184)
                                   11.20 WALL (11.18 usr +  0.00 sys = 11.18 CPU) @ 2732582.04/s (n=30562184)
                                   10.69 WALL (10.69 usr +  0.00 sys = 10.69 CPU) @ 2859957.00/s (n=30562184)
                                   10.64 WALL (10.63 usr +  0.00 sys = 10.63 CPU) @ 2875569.92/s (n=30562184)
                                   11.24 WALL (11.24 usr +  0.00 sys = 11.24 CPU) @ 2720047.70/s (n=30562184)
          pack: to_bytes key_hash: 10.08 WALL (10.08 usr +  0.00 sys = 10.08 CPU) @ 1580551.18/s (n=15924864)
                                   10.41 WALL (10.40 usr +  0.00 sys = 10.40 CPU) @ 1600635.59/s (n=16639625)
                                   10.05 WALL (10.04 usr +  0.00 sys = 10.04 CPU) @ 1595830.10/s (n=16021989)
                                   10.06 WALL (10.04 usr +  0.00 sys = 10.04 CPU) @ 1599560.70/s (n=16063446)
                                   10.09 WALL (10.05 usr +  0.01 sys = 10.06 CPU) @ 1677849.36/s (n=16882938)
  pack: to_bytes address_implicit: 11.34 WALL (11.32 usr +  0.00 sys = 11.32 CPU) @ 1195679.61/s (n=13534689)
                                   10.05 WALL (10.02 usr +  0.00 sys = 10.03 CPU) @ 1363052.34/s (n=13665761)
                                   10.38 WALL (10.36 usr +  0.00 sys = 10.37 CPU) @ 1305742.95/s (n=13534689)
                                   10.04 WALL (10.00 usr +  0.00 sys = 10.00 CPU) @ 1366937.44/s (n=13670509)
                                   10.17 WALL (10.15 usr +  0.00 sys = 10.15 CPU) @ 1333049.78/s (n=13534689)
pack: to_bytes address_originated: 10.51 WALL (10.49 usr +  0.00 sys = 10.49 CPU) @ 2462428.29/s (n=25839575)
                                   12.06 WALL (12.04 usr +  0.00 sys = 12.04 CPU) @ 2145309.50/s (n=25839575)
                                   10.05 WALL (10.02 usr +  0.00 sys = 10.02 CPU) @ 2892259.43/s (n=28992237)
                                   10.06 WALL (10.05 usr +  0.00 sys = 10.05 CPU) @ 2731348.24/s (n=27449468)
                                   10.01 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 2856762.27/s (n=28592228)

Benchmark pack 
                                         Rate           pack: to_bytes address_implicit pack: to_bytes key_hash pack: to_bytes pair pack: address implicit pack: key hash pack: to_bytes address_originated pack: to_bytes key pack: to_bytes bytes pack: to_bytes list pack: to_bytes int pack: address originated pack: key pack: (1, (0xAA, -1)) pack: bytes 050001 pack: list pair pack: pair (1, 0x) pack: bytes 0x pack: list int 1 pack: list empty pack: int 1 pack: int -1
  pack: to_bytes address_implicit   1312892+-   59530/s                              --                    -18%                -29%                   -36%           -47%                              -50%               -53%                 -68%                -77%               -77%                     -78%      -84%                  -89%               -89%            -91%               -92%           -96%             -99%            -100%       -100%        -100%
          pack: to_bytes key_hash   1610885+-   32535/s                             23%                      --                -13%                   -21%           -35%                              -38%               -43%                 -61%                -71%               -72%                     -73%      -81%                  -86%               -87%            -89%               -91%           -95%             -99%            -100%       -100%        -100%
              pack: to_bytes pair   1846762+-   82741/s                             41%                     15%                  --                   -10%           -26%                              -29%               -35%                 -56%                -67%               -68%                     -69%      -78%                  -85%               -85%            -87%               -89%           -94%             -99%             -99%       -100%        -100%
           pack: address implicit   2049073+-   93038/s                             56%                     27%                 11%                     --           -18%                              -22%               -27%                 -51%                -63%               -64%                     -66%      -75%                  -83%               -83%            -86%               -88%           -94%             -99%             -99%        -99%         -99%
                   pack: key hash   2496323+-  134204/s                             90%                     55%                 35%                    22%             --                             [-5%]               -11%                 -40%                -55%               -56%                     -58%      -70%                  -79%               -80%            -83%               -85%           -92%             -99%             -99%        -99%         -99%
pack: to_bytes address_originated   2617622+-  266241/s                             99%                     62%                 42%                    28%           [5%]                                --              [-7%]                 -37%                -53%               -54%                     -56%      -69%                  -78%               -79%            -82%               -85%           -92%             -99%             -99%        -99%         -99%
               pack: to_bytes key   2820406+-   74957/s                            115%                     75%                 53%                    38%            13%                              [8%]                 --                 -32%                -50%               -51%                     -53%      -66%                  -76%               -77%            -81%               -84%           -91%             -99%             -99%        -99%         -99%
             pack: to_bytes bytes   4152202+-  167415/s                            216%                    158%                125%                   103%            66%                               59%                47%                   --                -26%               -27%                     -30%      -50%                  -65%               -66%            -72%               -76%           -87%             -98%             -99%        -99%         -99%
              pack: to_bytes list   5609242+-  291409/s                            327%                    248%                204%                   174%           125%                              114%                99%                  35%                  --              [-2%]                    [-6%]      -33%                  -53%               -55%            -62%               -67%           -83%             -97%             -98%        -99%         -99%
               pack: to_bytes int   5705282+-  387937/s                            335%                    254%                209%                   178%           129%                              118%               102%                  37%                [2%]                 --                    [-4%]      -32%                  -52%               -54%            -61%               -67%           -83%             -97%             -98%        -99%         -99%
         pack: address originated   5941380+-  532249/s                            353%                    269%                222%                   190%           138%                              127%               111%                  43%                [6%]               [4%]                       --      -29%                  -50%               -52%            -60%               -65%           -82%             -97%             -98%        -98%         -98%
                        pack: key   8355714+-  478767/s                            536%                    419%                352%                   308%           235%                              219%               196%                 101%                 49%                46%                      41%        --                  -30%               -32%            -43%               -51%           -74%             -96%             -98%        -98%         -98%
            pack: (1, (0xAA, -1))  11924682+-  569597/s                            808%                    640%                546%                   482%           378%                              356%               323%                 187%                113%               109%                     101%       43%                    --              [-3%]            -19%               -30%           -64%             -94%             -97%        -97%         -97%
               pack: bytes 050001  12355305+-  773269/s                            841%                    667%                569%                   503%           395%                              372%               338%                 198%                120%               117%                     108%       48%                  [4%]                 --            -16%               -28%           -62%             -93%             -97%        -97%         -97%
                  pack: list pair  14682693+-  793399/s                           1018%                    811%                695%                   617%           488%                              461%               421%                 254%                162%               157%                     147%       76%                   23%                19%              --             [-14%]           -55%             -92%             -96%        -96%         -96%
               pack: pair (1, 0x)  17105113+- 1870229/s                           1203%                    962%                826%                   735%           585%                              553%               506%                 312%                205%               200%                     188%      105%                   43%                38%           [16%]                 --           -48%             -91%             -95%        -96%         -96%
                   pack: bytes 0x  32761879+- 1573233/s                           2395%                   1934%               1674%                  1499%          1212%                             1152%              1062%                 689%                484%               474%                     451%      292%                  175%               165%            123%                92%             --             -83%             -91%        -92%         -92%
                 pack: list int 1 188162558+-24758141/s                          14232%                  11581%              10089%                  9083%          7438%                             7088%              6571%                4432%               3255%              3198%                    3067%     2152%                 1478%              1423%           1182%              1000%           474%               --             -49%        -51%         -51%
                 pack: list empty 367417088+-18999947/s                          27885%                  22708%              19795%                 17831%         14618%                            13936%             12927%                8749%               6450%              6340%                    6084%     4297%                 2981%              2874%           2402%              2048%          1021%              95%               --       [-5%]        [-5%]
                      pack: int 1 385801306+-29655747/s                          29286%                  23850%              20791%                 18728%         15355%                            14639%             13579%                9191%               6778%              6662%                    6393%     4517%                 3135%              3023%           2528%              2155%          1078%             105%             [5%]          --        [-0%]
                     pack: int -1 386813797+-26081033/s                          29363%                  23912%              20846%                 18777%         15395%                            14677%             13615%                9216%               6796%              6680%                    6411%     4529%                 3144%              3031%           2534%              2161%          1081%             106%             [5%]        [0%]           --
Latencies for 20000 iterations of "pack: int 1", "pack: int -1", "pack: bytes 0x", "pack: bytes 050001", "pack: pair (1, 0x)", "pack: (1, (0xAA, -1))", "pack: list empty", "pack: list int 1", "pack: list pair", "pack: key", "pack: key hash", "pack: address implicit", "pack: address originated", "pack: to_bytes int", "pack: to_bytes bytes", "pack: to_bytes pair", "pack: to_bytes list", "pack: to_bytes key", "pack: to_bytes key_hash", "pack: to_bytes address_implicit", "pack: to_bytes address_originated":
                      pack: int 1:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 327868853.90/s (n=20000)
                                   (warning: too few iterations for a reliable count)
                     pack: int -1:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 217391303.82/s (n=20000)
                                   (warning: too few iterations for a reliable count)
                   pack: bytes 0x:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 23752969.12/s (n=20000)
                                   (warning: too few iterations for a reliable count)
               pack: bytes 050001:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 8143322.48/s (n=20000)
                                   (warning: too few iterations for a reliable count)
               pack: pair (1, 0x):  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 13783597.52/s (n=20000)
                                   (warning: too few iterations for a reliable count)
            pack: (1, (0xAA, -1)):  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 7927070.95/s (n=20000)
                                   (warning: too few iterations for a reliable count)
                 pack: list empty:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 256410256.31/s (n=20000)
                                   (warning: too few iterations for a reliable count)
                 pack: list int 1:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 123456789.92/s (n=20000)
                                   (warning: too few iterations for a reliable count)
                  pack: list pair:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 8837825.89/s (n=20000)
                                   (warning: too few iterations for a reliable count)
                        pack: key:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 5546311.70/s (n=20000)
                                   (warning: too few iterations for a reliable count)
                   pack: key hash:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1698658.06/s (n=20000)
                                   (warning: too few iterations for a reliable count)
           pack: address implicit:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1154134.69/s (n=20000)
                                   (warning: too few iterations for a reliable count)
         pack: address originated:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 4006410.26/s (n=20000)
                                   (warning: too few iterations for a reliable count)
               pack: to_bytes int:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 6637902.42/s (n=20000)
                                   (warning: too few iterations for a reliable count)
             pack: to_bytes bytes:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 3864734.30/s (n=20000)
                                   (warning: too few iterations for a reliable count)
              pack: to_bytes pair:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1157139.55/s (n=20000)
                                   (warning: too few iterations for a reliable count)
              pack: to_bytes list:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 6044122.09/s (n=20000)
                                   (warning: too few iterations for a reliable count)
               pack: to_bytes key:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 3121098.63/s (n=20000)
                                   (warning: too few iterations for a reliable count)
          pack: to_bytes key_hash:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1088139.28/s (n=20000)
                                   (warning: too few iterations for a reliable count)
  pack: to_bytes address_implicit:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 982994.20/s (n=20000)
                                   (warning: too few iterations for a reliable count)
pack: to_bytes address_originated:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 2295947.65/s (n=20000)
                                   (warning: too few iterations for a reliable count)

                                         Rate pack: to_bytes address_implicit pack: to_bytes key_hash pack: address implicit pack: to_bytes pair pack: key hash pack: to_bytes address_originated pack: to_bytes key pack: to_bytes bytes pack: address originated pack: key pack: to_bytes list pack: to_bytes int pack: (1, (0xAA, -1)) pack: bytes 050001 pack: list pair pack: pair (1, 0x) pack: bytes 0x pack: list int 1 pack: int -1 pack: list empty pack: int 1
  pack: to_bytes address_implicit    982994/s                              --                    -10%                   -15%                -15%           -42%                              -57%               -69%                 -75%                     -75%      -82%                -84%               -85%                  -88%               -88%            -89%               -93%           -96%             -99%        -100%            -100%       -100%
          pack: to_bytes key_hash   1088139/s                             11%                      --                    -6%                 -6%           -36%                              -53%               -65%                 -72%                     -73%      -80%                -82%               -84%                  -86%               -87%            -88%               -92%           -95%             -99%         -99%            -100%       -100%
           pack: address implicit   1154135/s                             17%                      6%                     --                 -0%           -32%                              -50%               -63%                 -70%                     -71%      -79%                -81%               -83%                  -85%               -86%            -87%               -92%           -95%             -99%         -99%            -100%       -100%
              pack: to_bytes pair   1157140/s                             18%                      6%                     0%                  --           -32%                              -50%               -63%                 -70%                     -71%      -79%                -81%               -83%                  -85%               -86%            -87%               -92%           -95%             -99%         -99%            -100%       -100%
                   pack: key hash   1698658/s                             73%                     56%                    47%                 47%             --                              -26%               -46%                 -56%                     -58%      -69%                -72%               -74%                  -79%               -79%            -81%               -88%           -93%             -99%         -99%             -99%        -99%
pack: to_bytes address_originated   2295948/s                            134%                    111%                    99%                 98%            35%                                --               -26%                 -41%                     -43%      -59%                -62%               -65%                  -71%               -72%            -74%               -83%           -90%             -98%         -99%             -99%        -99%
               pack: to_bytes key   3121099/s                            218%                    187%                   170%                170%            84%                               36%                 --                 -19%                     -22%      -44%                -48%               -53%                  -61%               -62%            -65%               -77%           -87%             -97%         -99%             -99%        -99%
             pack: to_bytes bytes   3864734/s                            293%                    255%                   235%                234%           128%                               68%                24%                   --                      -4%      -30%                -36%               -42%                  -51%               -53%            -56%               -72%           -84%             -97%         -98%             -98%        -99%
         pack: address originated   4006410/s                            308%                    268%                   247%                246%           136%                               74%                28%                   4%                       --      -28%                -34%               -40%                  -49%               -51%            -55%               -71%           -83%             -97%         -98%             -98%        -99%
                        pack: key   5546312/s                            464%                    410%                   381%                379%           227%                              142%                78%                  44%                      38%        --                 -8%               -16%                  -30%               -32%            -37%               -60%           -77%             -96%         -97%             -98%        -98%
              pack: to_bytes list   6044122/s                            515%                    455%                   424%                422%           256%                              163%                94%                  56%                      51%        9%                  --                -9%                  -24%               -26%            -32%               -56%           -75%             -95%         -97%             -98%        -98%
               pack: to_bytes int   6637902/s                            575%                    510%                   475%                474%           291%                              189%               113%                  72%                      66%       20%                 10%                 --                  -16%               -18%            -25%               -52%           -72%             -95%         -97%             -97%        -98%
            pack: (1, (0xAA, -1))   7927071/s                            706%                    628%                   587%                585%           367%                              245%               154%                 105%                      98%       43%                 31%                19%                    --                -3%            -10%               -42%           -67%             -94%         -96%             -97%        -98%
               pack: bytes 050001   8143322/s                            728%                    648%                   606%                604%           379%                              255%               161%                 111%                     103%       47%                 35%                23%                    3%                 --             -8%               -41%           -66%             -93%         -96%             -97%        -98%
                  pack: list pair   8837826/s                            799%                    712%                   666%                664%           420%                              285%               183%                 129%                     121%       59%                 46%                33%                   11%                 9%              --               -36%           -63%             -93%         -96%             -97%        -97%
               pack: pair (1, 0x)  13783598/s                           1302%                   1167%                  1094%               1091%           711%                              500%               342%                 257%                     244%      149%                128%               108%                   74%                69%             56%                 --           -42%             -89%         -94%             -95%        -96%
                   pack: bytes 0x  23752969/s                           2316%                   2083%                  1958%               1953%          1298%                              935%               661%                 515%                     493%      328%                293%               258%                  200%               192%            169%                72%             --             -81%         -89%             -91%        -93%
                 pack: list int 1 123456790/s                          12459%                  11246%                 10597%              10569%          7168%                             5277%              3856%                3094%                    2981%     2126%               1943%              1760%                 1457%              1416%           1297%               796%           420%               --         -43%             -52%        -62%
                     pack: int -1 217391304/s                          22015%                  19878%                 18736%              18687%         12698%                             9368%              6865%                5525%                    5326%     3820%               3497%              3175%                 2642%              2570%           2360%              1477%           815%              76%           --             -15%        -34%
                 pack: list empty 256410256/s                          25985%                  23464%                 22117%              22059%         14995%                            11068%              8115%                6535%                    6300%     4523%               4142%              3763%                 3135%              3049%           2801%              1760%           979%             108%          18%               --        -22%
                      pack: int 1 327868854/s                          33254%                  30031%                 28308%              28234%         19202%                            14180%             10405%                8384%                    8084%     5811%               5325%              4839%                 4036%              3926%           3610%              2279%          1280%             166%          51%              28%          --
Throughputs for "consensus: key_hash_exn", "consensus: address_exn", "consensus: hash_validators", "consensus: hash_block", "consensus: hash_withdraw_handle" each running 5 times for at least 10 CPU seconds:
        consensus: key_hash_exn: 10.44 WALL (10.44 usr +  0.00 sys = 10.44 CPU) @ 289326.54/s (n=3019430)
                                 10.74 WALL (10.72 usr +  0.00 sys = 10.72 CPU) @ 281782.22/s (n=3019430)
                                 10.86 WALL (10.85 usr +  0.00 sys = 10.85 CPU) @ 278360.90/s (n=3019430)
                                 11.47 WALL (11.45 usr +  0.00 sys = 11.45 CPU) @ 263708.37/s (n=3019430)
                                 11.09 WALL (11.07 usr +  0.00 sys = 11.07 CPU) @ 272636.04/s (n=3019430)
         consensus: address_exn: 10.25 WALL (10.23 usr +  0.00 sys = 10.23 CPU) @ 261196.56/s (n=2671916)
                                 11.26 WALL (11.26 usr +  0.00 sys = 11.26 CPU) @ 221757.55/s (n=2496682)
                                 10.22 WALL (10.16 usr +  0.01 sys = 10.17 CPU) @ 245604.02/s (n=2496682)
                                 10.01 WALL (10.00 usr +  0.00 sys = 10.00 CPU) @ 279058.43/s (n=2791566)
                                 10.19 WALL (10.14 usr +  0.01 sys = 10.15 CPU) @ 245980.24/s (n=2496682)
     consensus: hash_validators: 12.00 WALL (11.96 usr +  0.01 sys = 11.97 CPU) @ 58246.32/s (n=697100)
                                 10.90 WALL (10.89 usr +  0.00 sys = 10.89 CPU) @ 63997.65/s (n=697100)
                                 11.38 WALL (11.38 usr +  0.00 sys = 11.38 CPU) @ 61276.87/s (n=697100)
                                 11.95 WALL (11.94 usr +  0.00 sys = 11.94 CPU) @ 58380.81/s (n=697100)
                                 10.97 WALL (10.94 usr +  0.00 sys = 10.94 CPU) @ 63696.92/s (n=697100)
          consensus: hash_block: 11.22 WALL (11.20 usr +  0.00 sys = 11.21 CPU) @ 237906.70/s (n=2665973)
                                 10.46 WALL (10.45 usr +  0.00 sys = 10.45 CPU) @ 255138.49/s (n=2665973)
                                 10.34 WALL (10.33 usr +  0.00 sys = 10.33 CPU) @ 258088.03/s (n=2665973)
                                 11.66 WALL (11.64 usr +  0.00 sys = 11.64 CPU) @ 229103.44/s (n=2665973)
                                 10.61 WALL (10.61 usr +  0.00 sys = 10.61 CPU) @ 251300.25/s (n=2665973)
consensus: hash_withdraw_handle: 10.03 WALL (10.02 usr +  0.00 sys = 10.02 CPU) @ 48391.02/s (n=485084)
                                 10.08 WALL (10.08 usr +  0.00 sys = 10.08 CPU) @ 49827.38/s (n=502053)
                                 10.49 WALL (10.48 usr +  0.00 sys = 10.48 CPU) @ 49130.07/s (n=515009)
                                 10.34 WALL (10.29 usr +  0.00 sys = 10.29 CPU) @ 46518.46/s (n=478831)
                                 10.05 WALL (10.04 usr +  0.00 sys = 10.04 CPU) @ 53047.67/s (n=532590)

Benchmark forge transaction 
                                    Rate        consensus: hash_withdraw_handle consensus: hash_validators consensus: hash_block consensus: address_exn consensus: key_hash_exn
consensus: hash_withdraw_handle  49383+- 2032/s                              --                       -19%                  -80%                   -80%                    -82%
     consensus: hash_validators  61120+- 2354/s                             24%                         --                  -75%                   -76%                    -78%
          consensus: hash_block 246307+-10478/s                            399%                       303%                    --                  [-2%]                    -11%
         consensus: address_exn 250719+-18026/s                            408%                       310%                  [2%]                     --                    -10%
        consensus: key_hash_exn 277163+- 8197/s                            461%                       353%                   13%                    11%                      --
Latencies for 20000 iterations of "consensus: key_hash_exn", "consensus: address_exn", "consensus: hash_validators", "consensus: hash_block", "consensus: hash_withdraw_handle":
        consensus: key_hash_exn:  0.07 WALL ( 0.07 usr +  0.00 sys =  0.07 CPU) @ 292894.38/s (n=20000)
                                 (warning: too few iterations for a reliable count)
         consensus: address_exn:  0.07 WALL ( 0.07 usr +  0.00 sys =  0.07 CPU) @ 286372.94/s (n=20000)
                                 (warning: too few iterations for a reliable count)
     consensus: hash_validators:  0.28 WALL ( 0.28 usr +  0.00 sys =  0.28 CPU) @ 70792.45/s (n=20000)
                                 (warning: too few iterations for a reliable count)
          consensus: hash_block:  0.08 WALL ( 0.08 usr +  0.00 sys =  0.08 CPU) @ 242297.95/s (n=20000)
                                 (warning: too few iterations for a reliable count)
consensus: hash_withdraw_handle:  0.35 WALL ( 0.35 usr +  0.00 sys =  0.35 CPU) @ 56365.66/s (n=20000)
                                 (warning: too few iterations for a reliable count)

                                    Rate consensus: hash_withdraw_handle consensus: hash_validators consensus: hash_block consensus: address_exn consensus: key_hash_exn
consensus: hash_withdraw_handle  56366/s                              --                       -20%                  -77%                   -80%                    -81%
     consensus: hash_validators  70792/s                             26%                         --                  -71%                   -75%                    -76%
          consensus: hash_block 242298/s                            330%                       242%                    --                   -15%                    -17%
         consensus: address_exn 286373/s                            408%                       305%                   18%                     --                     -2%
        consensus: key_hash_exn 292894/s                            420%                       314%                   21%                     2%                      --
Throughputs for "discovery" running 5 times for at least 10 CPU seconds:
discovery: 10.57 WALL (10.57 usr +  0.00 sys = 10.57 CPU) @ 17630.18/s (n=186359)
           10.60 WALL (10.60 usr +  0.00 sys = 10.60 CPU) @ 17584.81/s (n=186359)
           10.69 WALL (10.69 usr +  0.00 sys = 10.69 CPU) @ 17431.85/s (n=186359)
           10.55 WALL (10.55 usr +  0.00 sys = 10.55 CPU) @ 17668.94/s (n=186359)
           11.71 WALL (11.70 usr +  0.00 sys = 11.70 CPU) @ 15923.26/s (n=186359)

Benchmark forge transaction 
             Rate      discovery
discovery 17248+-634/s        --
Latencies for 20000 iterations of "discovery":
discovery:  1.50 WALL ( 1.49 usr +  0.00 sys =  1.49 CPU) @ 13382.46/s (n=20000)

             Rate discovery
discovery 13382/s        --
```

## Validators

Source `bench_time_validators.ml`

```
Throughputs for "setup", "setup two", "make", "current", "to_list", "length", "remove", "after_current", "update_current", "hash" each running 5 times for at least 10 CPU seconds:
         setup: 10.88 WALL (10.88 usr +  0.00 sys = 10.88 CPU) @ 37093.13/s (n=403398)
                12.92 WALL (12.89 usr +  0.00 sys = 12.90 CPU) @ 31279.59/s (n=403398)
                13.59 WALL (13.57 usr +  0.00 sys = 13.57 CPU) @ 29721.85/s (n=403398)
                13.53 WALL (13.51 usr +  0.00 sys = 13.52 CPU) @ 29843.29/s (n=403398)
                14.90 WALL (14.89 usr +  0.00 sys = 14.89 CPU) @ 27100.85/s (n=403398)
     setup two: 12.10 WALL (12.05 usr +  0.00 sys = 12.05 CPU) @ 13344.79/s (n=160863)
                10.82 WALL (10.79 usr +  0.00 sys = 10.79 CPU) @ 14908.31/s (n=160863)
                10.76 WALL (10.70 usr +  0.00 sys = 10.70 CPU) @ 15030.33/s (n=160863)
                10.74 WALL (10.70 usr +  0.00 sys = 10.70 CPU) @ 15034.05/s (n=160863)
                13.64 WALL (13.57 usr +  0.00 sys = 13.57 CPU) @ 11851.59/s (n=160863)
          make: 11.82 WALL (11.76 usr +  0.00 sys = 11.77 CPU) @ 30145.58/s (n=354665)
                10.86 WALL (10.81 usr +  0.00 sys = 10.81 CPU) @ 32795.62/s (n=354665)
                11.43 WALL (11.40 usr +  0.01 sys = 11.40 CPU) @ 31100.10/s (n=354665)
                11.36 WALL (11.34 usr +  0.00 sys = 11.34 CPU) @ 31270.72/s (n=354665)
                11.81 WALL (11.80 usr +  0.00 sys = 11.80 CPU) @ 30053.31/s (n=354665)
       current: 11.33 WALL (11.32 usr +  0.00 sys = 11.32 CPU) @ 29346.96/s (n=332212)
                10.47 WALL (10.47 usr +  0.00 sys = 10.47 CPU) @ 31726.89/s (n=332212)
                10.47 WALL (10.45 usr +  0.00 sys = 10.45 CPU) @ 31800.78/s (n=332212)
                10.35 WALL (10.34 usr +  0.00 sys = 10.34 CPU) @ 32136.93/s (n=332212)
                12.05 WALL (12.03 usr +  0.01 sys = 12.04 CPU) @ 27598.97/s (n=332212)
       to_list: 10.10 WALL (10.09 usr +  0.00 sys = 10.09 CPU) @ 30627.32/s (n=309005)
                10.09 WALL (10.09 usr +  0.00 sys = 10.09 CPU) @ 30377.66/s (n=306575)
                10.12 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 31992.41/s (n=323274)
                10.13 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 31885.03/s (n=322539)
                10.29 WALL (10.28 usr +  0.00 sys = 10.28 CPU) @ 27972.01/s (n=287455)
        length: 10.94 WALL (10.89 usr +  0.00 sys = 10.89 CPU) @ 26773.36/s (n=291433)
                10.03 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 29547.89/s (n=295755)
                10.14 WALL (10.12 usr +  0.00 sys = 10.12 CPU) @ 29654.81/s (n=300123)
                10.14 WALL (10.10 usr +  0.01 sys = 10.11 CPU) @ 29233.65/s (n=295529)
                10.11 WALL (10.08 usr +  0.00 sys = 10.08 CPU) @ 32559.25/s (n=328304)
        remove: 11.19 WALL (11.18 usr +  0.00 sys = 11.18 CPU) @ 14120.97/s (n=157839)
                13.14 WALL (13.10 usr +  0.01 sys = 13.11 CPU) @ 12039.07/s (n=157839)
                11.67 WALL (11.61 usr +  0.01 sys = 11.62 CPU) @ 13588.24/s (n=157839)
                10.28 WALL (10.26 usr +  0.00 sys = 10.26 CPU) @ 15377.13/s (n=157839)
                10.37 WALL (10.37 usr +  0.00 sys = 10.37 CPU) @ 15225.67/s (n=157839)
 after_current: 10.11 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 186521974.69/s (n=1885511286)
                10.18 WALL (10.15 usr +  0.01 sys = 10.16 CPU) @ 200214043.48/s (n=2034551885)
                10.57 WALL (10.52 usr +  0.01 sys = 10.53 CPU) @ 178986858.43/s (n=1885511286)
                10.14 WALL (10.14 usr +  0.00 sys = 10.14 CPU) @ 196669618.19/s (n=1993532538)
                10.06 WALL (10.05 usr +  0.00 sys = 10.06 CPU) @ 214373800.75/s (n=2155672197)
update_current: 10.09 WALL (10.08 usr +  0.00 sys = 10.08 CPU) @ 16683.39/s (n=168240)
                10.01 WALL (10.01 usr +  0.00 sys = 10.01 CPU) @ 17646.84/s (n=176669)
                10.00 WALL (10.00 usr +  0.00 sys = 10.00 CPU) @ 17838.53/s (n=178414)
                10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 17657.98/s (n=178420)
                10.09 WALL (10.07 usr +  0.00 sys = 10.08 CPU) @ 16621.46/s (n=167491)
          hash: 12.24 WALL (12.21 usr +  0.00 sys = 12.22 CPU) @ 14855.09/s (n=181503)
                11.28 WALL (11.23 usr +  0.00 sys = 11.23 CPU) @ 16162.15/s (n=181503)
                10.93 WALL (10.91 usr +  0.00 sys = 10.91 CPU) @ 16633.93/s (n=181503)
                10.50 WALL (10.49 usr +  0.00 sys = 10.49 CPU) @ 17296.18/s (n=181503)
                12.03 WALL (11.96 usr +  0.01 sys = 11.98 CPU) @ 15155.41/s (n=181503)

Benchmark validators 
                      Rate           setup two   remove     hash update_current  length current to_list   setup    make after_current
     setup two     14034+-    1201/s        --    [-0%]     -12%           -19%    -53%    -54%    -54%    -55%    -55%         -100%
        remove     14070+-    1156/s      [0%]       --     -12%           -19%    -52%    -54%    -54%    -55%    -55%         -100%
          hash     16021+-     863/s       14%      14%       --            -7%    -46%    -48%    -48%    -48%    -48%         -100%
update_current     17290+-     499/s       23%      23%       8%             --    -41%    -43%    -43%    -44%    -44%         -100%
        length     29554+-    1745/s      111%     110%      84%            71%      --   [-3%]   [-3%]   [-5%]   [-5%]         -100%
       current     30522+-    1679/s      117%     117%      91%            77%    [3%]      --   [-0%]   [-2%]   [-2%]         -100%
       to_list     30571+-    1379/s      118%     117%      91%            77%    [3%]    [0%]      --   [-1%]   [-2%]         -100%
         setup     31008+-    3162/s      121%     120%      94%            79%    [5%]    [2%]    [1%]      --   [-0%]         -100%
          make     31073+-     941/s      121%     121%      94%            80%    [5%]    [2%]    [2%]    [0%]      --         -100%
 after_current 195353259+-11502391/s  1391918% 1388317% 1219291%       1129786% 660909% 639939% 638917% 629914% 628590%            --
Latencies for 20000 iterations of "setup", "setup two", "make", "current", "to_list", "length", "remove", "after_current", "update_current", "hash":
         setup:  0.74 WALL ( 0.74 usr +  0.00 sys =  0.74 CPU) @ 27177.24/s (n=20000)
     setup two:  1.35 WALL ( 1.35 usr +  0.00 sys =  1.35 CPU) @ 14789.67/s (n=20000)
          make:  0.61 WALL ( 0.61 usr +  0.00 sys =  0.61 CPU) @ 32985.23/s (n=20000)
       current:  0.63 WALL ( 0.63 usr +  0.00 sys =  0.63 CPU) @ 31590.74/s (n=20000)
       to_list:  0.62 WALL ( 0.62 usr +  0.00 sys =  0.62 CPU) @ 32288.90/s (n=20000)
        length:  0.63 WALL ( 0.63 usr +  0.00 sys =  0.63 CPU) @ 31759.79/s (n=20000)
        remove:  1.28 WALL ( 1.27 usr +  0.00 sys =  1.27 CPU) @ 15722.08/s (n=20000)
 after_current:  0.00 WALL ( 0.00 usr +  0.00 sys =  0.00 CPU) @ 168067226.67/s (n=20000)
                (warning: too few iterations for a reliable count)
update_current:  1.23 WALL ( 1.22 usr +  0.00 sys =  1.22 CPU) @ 16454.70/s (n=20000)
          hash:  1.21 WALL ( 1.20 usr +  0.00 sys =  1.20 CPU) @ 16615.92/s (n=20000)

                      Rate setup two   remove update_current     hash   setup current  length to_list    make after_current
     setup two     14790/s        --      -6%           -10%     -11%    -46%    -53%    -53%    -54%    -55%         -100%
        remove     15722/s        6%       --            -4%      -5%    -42%    -50%    -50%    -51%    -52%         -100%
update_current     16455/s       11%       5%             --      -1%    -39%    -48%    -48%    -49%    -50%         -100%
          hash     16616/s       12%       6%             1%       --    -39%    -47%    -48%    -49%    -50%         -100%
         setup     27177/s       84%      73%            65%      64%      --    -14%    -14%    -16%    -18%         -100%
       current     31591/s      114%     101%            92%      90%     16%      --     -1%     -2%     -4%         -100%
        length     31760/s      115%     102%            93%      91%     17%      1%      --     -2%     -4%         -100%
       to_list     32289/s      118%     105%            96%      94%     19%      2%      2%      --     -2%         -100%
          make     32985/s      123%     110%           100%      99%     21%      4%      4%      2%      --         -100%
 after_current 168067227/s  1136282% 1068888%       1021293% 1011383% 618312% 531914% 529082% 520411% 509423%            --
```