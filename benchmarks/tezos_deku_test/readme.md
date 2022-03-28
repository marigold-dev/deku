# Benchmark test libraries 

## Alcotest vs Rely

~/dekuꜩ esy b dune exec ./benchmarks/tezos_deku_test/benchmarks_test_deku.exe ledger
Estimated testing time 20s (2 benchmarks x 10s). Change using '-quota'.
┌───────────────────┬──────────────┬────────────┬──────────┬──────────┬────────────┐
│ Name              │     Time/Run │    mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├───────────────────┼──────────────┼────────────┼──────────┼──────────┼────────────┤
│ Alcotest: balance │      10.29ns │     10.00w │          │          │            │
│ Rely: balance     │ 571_247.33ns │ 59_403.46w │  17.81kw │  17.81kw │    100.00% │
└───────────────────┴──────────────┴────────────┴──────────┴──────────┴────────────┘