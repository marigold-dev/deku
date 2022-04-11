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

[Benchmark documentation](https://chris00.github.io/ocaml-benchmark/doc/benchmark/Benchmark/index.html)

TODO

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

Throughputs for "Execute: add", "Execute: sub", "Execute: mul", "Execute: div", "Execute: rem", "Execute: land", "Execute: lor", "Execute: lxor", "Execute: lsl", "Execute: lsr", "Execute: asr" each running 5 times for at least 10 CPU seconds:
 Execute: add: 10.24 WALL (10.22 usr +  0.00 sys = 10.22 CPU) @ 2117820.95/s (n=21636849)
               10.17 WALL (10.16 usr +  0.00 sys = 10.16 CPU) @ 2129541.12/s (n=21636849)
               11.31 WALL (11.30 usr +  0.00 sys = 11.31 CPU) @ 1913666.65/s (n=21636849)
               11.33 WALL (11.31 usr +  0.00 sys = 11.31 CPU) @ 1912758.53/s (n=21636849)
               11.15 WALL (11.15 usr +  0.00 sys = 11.15 CPU) @ 1941139.82/s (n=21636849)
 Execute: sub: 10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 1779589.91/s (n=17976782)
               10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 1880192.03/s (n=18993102)
               10.12 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 1888832.17/s (n=19103692)
               10.09 WALL (10.09 usr +  0.00 sys = 10.09 CPU) @ 1961360.54/s (n=19788090)
               10.07 WALL (10.07 usr +  0.00 sys = 10.07 CPU) @ 1949582.58/s (n=19632669)
 Execute: mul: 11.14 WALL (11.13 usr +  0.00 sys = 11.13 CPU) @ 1779781.35/s (n=19814651)
               10.63 WALL (10.63 usr +  0.00 sys = 10.63 CPU) @ 1863325.25/s (n=19814651)
               10.51 WALL (10.50 usr +  0.00 sys = 10.50 CPU) @ 1886774.49/s (n=19814651)
               10.40 WALL (10.40 usr +  0.00 sys = 10.40 CPU) @ 1905070.81/s (n=19814651)
               10.04 WALL (10.04 usr +  0.00 sys = 10.04 CPU) @ 1973799.26/s (n=19814651)
 Execute: div: 11.08 WALL (11.07 usr +  0.00 sys = 11.07 CPU) @ 1904383.70/s (n=21088172)
               12.43 WALL (12.42 usr +  0.00 sys = 12.42 CPU) @ 1698156.17/s (n=21088172)
               11.27 WALL (11.26 usr +  0.00 sys = 11.26 CPU) @ 1872436.51/s (n=21088172)
               11.34 WALL (11.33 usr +  0.00 sys = 11.33 CPU) @ 1861149.23/s (n=21088172)
               10.90 WALL (10.90 usr +  0.00 sys = 10.90 CPU) @ 1934646.76/s (n=21088172)
 Execute: rem: 10.18 WALL (10.18 usr +  0.00 sys = 10.18 CPU) @ 1958794.20/s (n=19934104)
               11.81 WALL (11.80 usr +  0.00 sys = 11.80 CPU) @ 1689227.49/s (n=19934104)
               10.47 WALL (10.47 usr +  0.00 sys = 10.47 CPU) @ 1904675.38/s (n=19934104)
               10.87 WALL (10.87 usr +  0.00 sys = 10.87 CPU) @ 1834098.75/s (n=19934104)
               10.21 WALL (10.21 usr +  0.00 sys = 10.21 CPU) @ 1951943.32/s (n=19934104)
Execute: land: 10.50 WALL (10.49 usr +  0.01 sys = 10.49 CPU) @ 1960203.28/s (n=20571122)
               11.27 WALL (11.26 usr +  0.00 sys = 11.26 CPU) @ 1827010.63/s (n=20571122)
               11.58 WALL (11.56 usr +  0.01 sys = 11.56 CPU) @ 1778843.45/s (n=20571122)
               10.86 WALL (10.82 usr +  0.00 sys = 10.83 CPU) @ 1900008.48/s (n=20571122)
               11.02 WALL (10.98 usr +  0.00 sys = 10.99 CPU) @ 1872206.02/s (n=20571122)
 Execute: lor: 10.45 WALL (10.42 usr +  0.00 sys = 10.43 CPU) @ 1965839.45/s (n=20501203)
               10.37 WALL (10.37 usr +  0.00 sys = 10.37 CPU) @ 1977892.23/s (n=20501203)
               11.97 WALL (11.97 usr +  0.00 sys = 11.97 CPU) @ 1712968.38/s (n=20501203)
               10.55 WALL (10.54 usr +  0.00 sys = 10.54 CPU) @ 1944285.09/s (n=20501203)
               10.91 WALL (10.90 usr +  0.00 sys = 10.90 CPU) @ 1880661.25/s (n=20501203)
Execute: lxor: 10.19 WALL (10.18 usr +  0.00 sys = 10.18 CPU) @ 1975803.23/s (n=20114973)
               10.16 WALL (10.16 usr +  0.00 sys = 10.16 CPU) @ 1980366.34/s (n=20114973)
               11.28 WALL (11.26 usr +  0.00 sys = 11.26 CPU) @ 1786821.16/s (n=20114973)
               11.31 WALL (11.29 usr +  0.00 sys = 11.29 CPU) @ 1782042.57/s (n=20114973)
               10.64 WALL (10.61 usr +  0.00 sys = 10.61 CPU) @ 1895066.14/s (n=20114973)
 Execute: lsl: 10.57 WALL (10.57 usr +  0.00 sys = 10.57 CPU) @ 1890045.24/s (n=19977215)
               10.46 WALL (10.45 usr +  0.00 sys = 10.45 CPU) @ 1911348.61/s (n=19977215)
               10.37 WALL (10.36 usr +  0.00 sys = 10.36 CPU) @ 1927782.70/s (n=19977215)
               11.71 WALL (11.70 usr +  0.00 sys = 11.70 CPU) @ 1706771.71/s (n=19977215)
               10.55 WALL (10.54 usr +  0.00 sys = 10.54 CPU) @ 1894477.77/s (n=19977215)
 Execute: lsr: 10.10 WALL (10.09 usr +  0.00 sys = 10.09 CPU) @ 1843121.78/s (n=18595228)
               10.12 WALL (10.11 usr +  0.00 sys = 10.11 CPU) @ 1955201.34/s (n=19774224)
               10.11 WALL (10.10 usr +  0.00 sys = 10.10 CPU) @ 1924548.22/s (n=19446813)
               10.21 WALL (10.20 usr +  0.00 sys = 10.20 CPU) @ 1897990.09/s (n=19367015)
               10.08 WALL (10.08 usr +  0.00 sys = 10.08 CPU) @ 1807780.90/s (n=18213812)
 Execute: asr: 10.54 WALL (10.54 usr +  0.00 sys = 10.54 CPU) @ 2024791.06/s (n=21339354)
               10.66 WALL (10.66 usr +  0.00 sys = 10.66 CPU) @ 2002605.34/s (n=21339354)
               10.59 WALL (10.59 usr +  0.00 sys = 10.59 CPU) @ 2015848.79/s (n=21339354)
               10.57 WALL (10.56 usr +  0.00 sys = 10.56 CPU) @ 2020436.76/s (n=21339354)
               10.82 WALL (10.82 usr +  0.00 sys = 10.82 CPU) @ 1972509.56/s (n=21339354)

Bench two parameters
                   Rate        Execute: div Execute: lsl Execute: land Execute: rem Execute: mul Execute: lxor Execute: lsr Execute: sub Execute: lor Execute: add Execute: asr
 Execute: div 1854154+-78016/s           --        [-1%]         [-1%]        [-1%]        [-1%]         [-2%]        [-2%]        [-2%]        [-2%]          -7%          -8%
 Execute: lsl 1866085+-76728/s         [1%]           --         [-0%]        [-0%]        [-1%]         [-1%]        [-1%]        [-1%]        [-2%]        [-7%]          -7%
Execute: land 1867654+-58827/s         [1%]         [0%]            --        [-0%]        [-1%]         [-1%]        [-1%]        [-1%]        [-2%]          -7%          -7%
 Execute: rem 1867748+-94756/s         [1%]         [0%]          [0%]           --        [-1%]         [-1%]        [-1%]        [-1%]        [-2%]        [-7%]          -7%
 Execute: mul 1881750+-59751/s         [1%]         [1%]          [1%]         [1%]           --         [-0%]        [-0%]        [-1%]        [-1%]        [-6%]          -6%
Execute: lxor 1884020+-82465/s         [2%]         [1%]          [1%]         [1%]         [0%]            --        [-0%]        [-0%]        [-1%]        [-6%]          -6%
 Execute: lsr 1885728+-50938/s         [2%]         [1%]          [1%]         [1%]         [0%]          [0%]           --        [-0%]        [-1%]        [-6%]          -6%
 Execute: sub 1891911+-61438/s         [2%]         [1%]          [1%]         [1%]         [1%]          [0%]         [0%]           --        [-0%]        [-6%]          -6%
 Execute: lor 1896329+-92741/s         [2%]         [2%]          [2%]         [2%]         [1%]          [1%]         [1%]         [0%]           --        [-5%]        [-6%]
 Execute: add 2002985+-94186/s           8%         [7%]            7%         [7%]         [6%]          [6%]         [6%]         [6%]         [6%]           --        [-0%]
 Execute: asr 2007238+-17945/s           8%           8%            7%           7%           7%            7%           6%           6%         [6%]         [0%]           --
Latencies for 20000 iterations of "Execute: add", "Execute: sub", "Execute: mul", "Execute: div", "Execute: rem", "Execute: land", "Execute: lor", "Execute: lxor", "Execute: lsl", "Execute: lsr", "Execute: asr":
 Execute: add:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1629062.47/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: sub:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 978186.44/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: mul:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 947014.54/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: div:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1236705.42/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: rem:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1443209.70/s (n=20000)
               (warning: too few iterations for a reliable count)
Execute: land:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 994035.79/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: lor:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1072903.81/s (n=20000)
               (warning: too few iterations for a reliable count)
Execute: lxor:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1296260.29/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: lsl:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 1206854.94/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: lsr:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 981546.92/s (n=20000)
               (warning: too few iterations for a reliable count)
 Execute: asr:  0.02 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 1336451.72/s (n=20000)
               (warning: too few iterations for a reliable count)

                   Rate Execute: mul Execute: sub Execute: lsr Execute: land Execute: lor Execute: lsl Execute: div Execute: lxor Execute: asr Execute: rem Execute: add
 Execute: mul  947015/s           --          -3%          -4%           -5%         -12%         -22%         -23%          -27%         -29%         -34%         -42%
 Execute: sub  978186/s           3%           --          -0%           -2%          -9%         -19%         -21%          -25%         -27%         -32%         -40%
 Execute: lsr  981547/s           4%           0%           --           -1%          -9%         -19%         -21%          -24%         -27%         -32%         -40%
Execute: land  994036/s           5%           2%           1%            --          -7%         -18%         -20%          -23%         -26%         -31%         -39%
 Execute: lor 1072904/s          13%          10%           9%            8%           --         -11%         -13%          -17%         -20%         -26%         -34%
 Execute: lsl 1206855/s          27%          23%          23%           21%          12%           --          -2%           -7%         -10%         -16%         -26%
 Execute: div 1236705/s          31%          26%          26%           24%          15%           2%           --           -5%          -7%         -14%         -24%
Execute: lxor 1296260/s          37%          33%          32%           30%          21%           7%           5%            --          -3%         -10%         -20%
 Execute: asr 1336452/s          41%          37%          36%           34%          25%          11%           8%            3%           --          -7%         -18%
 Execute: rem 1443210/s          52%          48%          47%           45%          35%          20%          17%           11%           8%           --         -11%
 Execute: add 1629062/s          72%          67%          66%           64%          52%          35%          32%           26%          22%          13%           --
```