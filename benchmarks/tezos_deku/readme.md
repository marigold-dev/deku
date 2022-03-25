# Benchmark of Deku

Computer configuation that run the benchmark:

```
Memory: 15.3 GiB
Processor: Intel® Core™ i7-8665U CPU @ 1.90GHz × 8 
OS Name: Ubuntu 20.04.4 LTS
OS Type: 64-bit
GNOME version: 3.36.8
```

## Run
- dune:

    - Build: `dune build @benchmarks/bench_deku`

    - Execute: `dune exec -- ./benchmarks/tezos_deku/benchmarks_deku.exe subcommand`

Where `subcommand` is:
- `rpc`: for tezos rpc
- `ledger`: for ledger 
- `validators`: for validators
- `patricia`: for incremental patricia tree
- `interop`: for tezos internal operations 

For example: `dune exec -- ./benchmarks/tezos_deku/benchmarks_deku.exe ledger` will return the benchmark for ledger.

- esy:

    - Build: `esy b dune build`

    - Execute: `esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe subcommand`

## Ledger

Source `bench_ledger.ml`. It is a benchmark of functions defined in `src/core_deku/ledger.ml`.


```
~/dekuꜩ esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe ledger
Estimated testing time 1m50s (11 benchmarks x 10s). Change using '-quota'.
┌───────────────────────────────────────────┬──────────┬───────────┬──────────┬──────────┬────────────┐
│ Name                                      │ Time/Run │   mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────────────────────────────┼──────────┼───────────┼──────────┼──────────┼────────────┤
│ make ticket                               │   3.43us │   149.92w │   28.00w │   28.00w │      0.86% │
│ make address                              │  46.75us │   214.13w │   28.32w │   28.32w │     11.67% │
│ make tezos address                        │  59.13us │   205.44w │   27.96w │   27.96w │     14.77% │
│ one deposit: one ticket, one address      │  56.50us │   395.72w │   55.92w │   55.92w │     14.11% │
│ four deposits: two tickets, two addresses │ 125.01us │ 1_107.18w │  113.13w │  113.13w │     31.22% │
│ deposit: get balance                      │ 144.16us │   448.60w │   56.97w │   56.97w │     36.00% │
│ four deposits: get balance                │ 156.97us │ 1_371.55w │  112.70w │  112.70w │     39.20% │
│ transfer 1                                │ 273.54us │ 1_375.24w │  113.60w │  113.60w │     68.31% │
│ transfer 4                                │ 157.73us │ 2_294.43w │  113.69w │  113.69w │     39.39% │
│ withdraw 1                                │ 318.68us │ 2_231.21w │  143.14w │  143.14w │     79.58% │
│ withdraw 4                                │ 400.45us │ 5_191.53w │  172.34w │  172.34w │    100.00% │
└───────────────────────────────────────────┴──────────┴───────────┴──────────┴──────────┴────────────┘
```


## Validators

Source `bench_validators.ml`. It is a benchmark of functions defined in `src/protocol/validators.ml`


```
~/dekuꜩ esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe validators
Estimated testing time 1m40s (10 benchmarks x 10s). Change using '-quota'.
┌──────────────────────┬──────────────┬───────────┬──────────┬──────────┬────────────┐
│ Name                 │     Time/Run │   mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────────┼──────────────┼───────────┼──────────┼──────────┼────────────┤
│ make validator       │ 138_512.77ns │   215.55w │   29.05w │   29.05w │     27.97% │
│ setup one validator  │  59_777.95ns │   722.76w │   28.04w │   28.04w │     12.07% │
│ setup two validators │ 179_069.22ns │ 1_552.17w │   56.76w │   56.76w │     36.16% │
│ current validator    │  74_848.88ns │   727.07w │   28.20w │   28.20w │     15.11% │
│ to_list              │  58_424.35ns │   726.64w │   28.19w │   28.19w │     11.80% │
│ length               │  68_805.86ns │   724.40w │   28.09w │   28.09w │     13.89% │
│ remove validator     │ 164_190.49ns │ 1_821.04w │   56.84w │   56.84w │     33.15% │
│ after_current        │      27.98ns │     5.00w │          │          │            │
│ update_current       │ 235_630.52ns │ 1_576.85w │   57.11w │   57.11w │     47.58% │
│ hash                 │ 495_235.44ns │ 1_570.81w │   57.41w │   57.41w │    100.00% │
└──────────────────────┴──────────────┴───────────┴──────────┴──────────┴────────────┘
```

## Tezos RPC

Source `bench_tezos_rpc.ml`. It is a benchmark of functions defined in `src/tezos_rpc/`

TODO : run node


## Patricia

Source `bench_patricia.ml`. It is a benchmark for incremental Patricia tree, where the size of the tree is set to 10.


```
~/dekuꜩ esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe patricia
Estimated testing time 30s (3 benchmarks x 10s). Change using '-quota'.
┌───────────────────┬──────────┬───────────┬──────────┬──────────┬────────────┐
│ Name              │ Time/Run │   mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────┼──────────┼───────────┼──────────┼──────────┼────────────┤
│ add and find tree │   1.37us │   444.01w │    0.26w │    0.26w │      9.39% │
│ hash tree         │   8.23us │   798.18w │    0.18w │    0.18w │     56.32% │
│ hash values       │  14.62us │ 1_044.00w │    0.24w │    0.24w │    100.00% │
└───────────────────┴──────────┴───────────┴──────────┴──────────┴────────────┘
```


## Tezos internal operations

Source `bench_tezos_interop.ml`

TODO

```
~/dekuꜩ esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe interop
Estimated testing time 2m10s (13 benchmarks x 10s). Change using '-quota'.
┌──────────────────────────────────────┬─────────────┬─────────────┬─────────────┬─────────────┬────────────┐
│ Name                                 │    Time/Run │     mWd/Run │    mjWd/Run │    Prom/Run │ Percentage │
├──────────────────────────────────────┼─────────────┼─────────────┼─────────────┼─────────────┼────────────┤
│ key: to_string                       │    101.91us │   2_653.50w │     196.49w │     196.49w │      0.56% │
│ key: of_string                       │    934.02us │  32_543.89w │  18_416.21w │  18_416.21w │      5.12% │
│ key: hash                            │    164.26us │     464.24w │     133.42w │     133.42w │      0.90% │
│ secret: to_string                    │     40.81us │   2_427.22w │      91.23w │      91.23w │      0.22% │
│ secret: of_string                    │    172.94us │   6_366.30w │     197.27w │     197.27w │      0.95% │
│ verify signature                     │ 18_239.09us │ 248_880.21w │ 162_729.82w │ 162_729.82w │    100.00% │
│ contract hash: to_string             │     15.26us │     509.89w │      21.03w │      21.03w │      0.08% │
│ contract hash: of_string             │      8.02us │     655.21w │      21.09w │      21.09w │      0.04% │
│ address: to_string                   │     79.12us │   2_568.63w │     105.20w │     105.20w │      0.43% │
│ address: of_string                   │    100.16us │   8_799.69w │     232.21w │     232.21w │      0.55% │
│ ticket: to_string                    │     55.80us │   2_886.26w │   1_280.10w │      27.98w │      0.31% │
│ address: of_string                   │    142.79us │   8_804.22w │     232.34w │     232.34w │      0.78% │
│ operation hash: to_string, of_string │     36.23us │   1_720.13w │      42.23w │      42.23w │      0.20% │
└──────────────────────────────────────┴─────────────┴─────────────┴─────────────┴─────────────┴────────────┘
```