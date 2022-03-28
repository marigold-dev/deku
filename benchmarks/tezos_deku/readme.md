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


```
~/dekuꜩ esy b dune exec ./benchmarks/tezos_deku/benchmarks_deku.exe interop
Estimated testing time 7m10s (43 benchmarks x 10s). Change using '-quota'.
┌──────────────────────────────────────┬─────────────────┬─────────────┬─────────────┬─────────────┬────────────┐
│ Name                                 │        Time/Run │     mWd/Run │    mjWd/Run │    Prom/Run │ Percentage │
├──────────────────────────────────────┼─────────────────┼─────────────┼─────────────┼─────────────┼────────────┤
│ key: to_string                       │    149_074.63ns │   2_657.08w │     196.76w │     196.76w │      1.07% │
│ key: of_string                       │    929_113.50ns │  32_551.12w │  18_420.48w │  18_420.48w │      6.70% │
│ key: hash                            │     89_378.30ns │     463.82w │     133.29w │     133.29w │      0.64% │
│ secret: to_string                    │     29_057.61ns │   2_427.75w │      91.25w │      91.25w │      0.21% │
│ secret: of_string                    │     53_129.43ns │   6_357.94w │     197.00w │     197.00w │      0.38% │
│ verify signature                     │ 13_877_639.87ns │ 249_299.26w │ 163_005.96w │ 163_005.96w │    100.00% │
│ contract hash: to_string             │      6_840.48ns │     510.00w │      21.03w │      21.03w │      0.05% │
│ contract hash: of_string             │      6_386.63ns │     655.02w │      21.08w │      21.08w │      0.05% │
│ address: to_string                   │     35_188.41ns │   2_569.42w │     105.23w │     105.23w │      0.25% │
│ address: of_string                   │     86_673.99ns │   8_798.55w │     232.18w │     232.18w │      0.62% │
│ ticket: to_string                    │     27_761.34ns │   2_886.95w │   1_280.44w │      28.03w │      0.20% │
│ ticket: of_string                    │     32_018.94ns │   4_179.84w │      85.66w │      85.66w │      0.23% │
│ address: of_string                   │     79_700.51ns │   8_799.92w │     232.22w │     232.22w │      0.57% │
│ operation hash: to_string, of_string │     17_075.38ns │   1_719.06w │      42.20w │      42.20w │      0.12% │
│ forge transaction taquito            │    145_493.29ns │   3_970.93w │     161.77w │     161.77w │      1.05% │
│ forge transaction bytes              │    194_147.37ns │   5_901.79w │     225.00w │     225.00w │      1.40% │
│ pack: int 1                          │          5.79ns │       3.00w │             │             │            │
│ pack: int -1                         │          5.40ns │       3.00w │             │             │            │
│ pack: bytes 0x                       │         44.80ns │      10.00w │             │             │            │
│ pack: bytes 050001                   │        121.05ns │      20.00w │             │             │            │
│ pack: pair: (1, 0x)                  │         62.02ns │      24.00w │             │             │            │
│ pack: (1, (0xAA, -1))                │        108.99ns │      48.00w │             │             │            │
│ pack: list empty                     │          6.16ns │       3.00w │             │             │            │
│ pack: list int 1                     │          9.22ns │       9.00w │             │             │            │
│ pack: list pair                      │         82.13ns │      44.00w │             │             │            │
│ pack: key                            │        166.16ns │      48.00w │             │             │            │
│ pack: key_hash                       │        560.77ns │     100.02w │             │             │            │
│ pack: address implicit               │        897.14ns │     134.00w │             │             │            │
│ pack: address originated             │        298.10ns │      48.00w │             │             │            │
│ pack: to_bytes int                   │        271.04ns │      75.00w │             │             │            │
│ pack: to_bytes bytes                 │        399.51ns │      77.00w │             │             │            │
│ pack: to_bytes list                  │        223.02ns │      79.00w │             │             │            │
│ pack: to_bytes pair                  │        934.95ns │     162.98w │             │             │            │
│ pack: to_bytes key                   │        508.12ns │     123.01w │             │             │            │
│ pack: to_bytes key_hash              │        988.87ns │     173.00w │             │             │            │
│ pack: to_bytes address implicit      │      1_328.53ns │     207.00w │             │             │            │
│ pack: to_bytes address originated    │        524.70ns │     121.02w │             │             │            │
│ consensus: address_exn               │      7_855.68ns │     672.97w │      21.07w │      21.07w │      0.06% │
│ consensus: key_hash_exn              │      6_392.31ns │     666.89w │      21.03w │      21.03w │      0.05% │
│ consensus: hash_validators           │     29_767.32ns │   3_077.47w │      84.51w │      84.51w │      0.21% │
│ consensus: hash_block                │      7_088.17ns │     700.27w │       0.19w │       0.19w │      0.05% │
│ consensus: hash_withdraw_handle      │     31_144.40ns │   3_888.99w │     105.60w │     105.60w │      0.22% │
│ discovery                            │    123_859.03ns │   2_273.24w │      85.07w │      85.07w │      0.89% │
└──────────────────────────────────────┴─────────────────┴─────────────┴─────────────┴─────────────┴────────────┘
```