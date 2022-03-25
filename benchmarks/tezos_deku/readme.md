# Benchmark of Deku

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