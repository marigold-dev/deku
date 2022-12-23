# Run the benchmarks

```bash
$ dune exec benchmarking

Estimated testing time 2m40s (16 benchmarks x 10s). Change using '-quota'.
level to JSON/yojson: Total time taken 10.032530069351196s (1143 samples, max runs 1822631).
level to JSON/encoding: Total time taken 10.098333120346069s (1171 samples, max runs 2408211).
Encode_operation to JSON/yojson: Total time taken 10.000631093978882s (527 samples, max runs 4021).
Encode_operation to JSON/encoding: Total time taken 10.075474977493286s (648 samples, max runs 13285).
Operations to JSON/yojson: Total time taken 10.07265305519104s (469 samples, max runs 2279).
Operations to JSON/encoding: Total time taken 10.0344078540802s (538 samples, max runs 4481).
level from JSON/yojson: Total time taken 10.066639184951782s (976 samples, max runs 346003).
level from JSON/encoding: Total time taken 10.045109033584595s (909 samples, max runs 177666).
Encode_operation from JSON/yojson: Total time taken 10.079186201095581s (124 samples, max runs 124).
Encode_operation from JSON/encoding: Total time taken 10.10786509513855s (125 samples, max runs 125).
Operations from JSON/yojson: Total time taken 10.132002115249634s (124 samples, max runs 124).
Operations from JSON/encoding: Total time taken 10.004235744476318s (124 samples, max runs 124).
Operations from JSON + to_SIGNED/yojson: Total time taken 10.158401012420654s (122 samples, max runs 122).
Operations from JSON + to_SIGNED/encoding: Total time taken 10.160748958587646s (123 samples, max runs 123).
Operations from JSON + tup/encoding: Total time taken 10.052727937698364s (124 samples, max runs 124).
Operations from binary/encoding: Total time taken 10.023288249969482s (655 samples, max runs 14239).
┌───────────────────────────────────────────┬────────────────┬───────────────┬────────────┬────────────┬─────────────┬──────────┐
│ Name                                      │       Time/Run │       mWd/Run │   mjWd/Run │   Prom/Run │     mGC/Run │ mjGC/Run │
├───────────────────────────────────────────┼────────────────┼───────────────┼────────────┼────────────┼─────────────┼──────────┤
│ level to JSON/yojson                      │        54.92ns │        14.00w │            │            │     0.05e-3 │          │
│ level to JSON/encoding                    │        41.93ns │        49.00w │            │            │     0.19e-3 │          │
│ Encode_operation to JSON/yojson           │    23_961.44ns │    15_285.29w │    150.77w │    150.77w │    58.94e-3 │  0.63e-3 │
│ Encode_operation to JSON/encoding         │     7_480.26ns │     2_869.19w │     44.74w │     44.74w │    11.10e-3 │  0.15e-3 │
│ Operations to JSON/yojson                 │    41_533.79ns │    18_500.14w │    219.22w │    219.22w │    71.27e-3 │  0.70e-3 │
│ Operations to JSON/encoding               │    21_624.05ns │     5_912.69w │     88.59w │     88.59w │    22.81e-3 │  0.25e-3 │
│ level from JSON/yojson                    │       290.44ns │        96.00w │            │            │     0.37e-3 │          │
│ level from JSON/encoding                  │       563.94ns │       486.00w │      0.17w │      0.17w │     1.86e-3 │          │
│ Encode_operation from JSON/yojson         │ 1_322_918.03ns │ 1_548_192.73w │ 14_095.65w │ 14_095.65w │ 5_937.22e-3 │ 31.44e-3 │
│ Encode_operation from JSON/encoding       │ 1_306_432.95ns │ 1_547_962.53w │ 13_982.59w │ 13_982.59w │ 5_934.08e-3 │ 29.13e-3 │
│ Operations from JSON/yojson               │ 1_327_051.61ns │ 1_552_779.10w │ 14_413.98w │ 14_413.98w │ 5_951.41e-3 │ 28.18e-3 │
│ Operations from JSON/encoding             │ 1_309_895.89ns │ 1_553_628.50w │ 14_203.58w │ 14_203.58w │ 5_952.61e-3 │ 26.10e-3 │
│ Operations from JSON + to_SIGNED/yojson   │ 1_379_044.45ns │ 1_552_898.45w │ 14_461.49w │ 14_461.49w │ 5_948.82e-3 │ 25.05e-3 │
│ Operations from JSON + to_SIGNED/encoding │ 1_352_071.62ns │ 1_553_819.52w │ 14_252.51w │ 14_252.51w │ 5_950.50e-3 │ 23.32e-3 │
│ Operations from JSON + tup/encoding       │ 1_316_792.64ns │ 1_557_600.61w │ 14_198.52w │ 14_198.52w │ 5_963.62e-3 │ 22.02e-3 │
│ Operations from binary/encoding           │     6_952.12ns │     1_155.88w │     14.43w │     14.43w │     4.43e-3 │  0.02e-3 │
└───────────────────────────────────────────┴────────────────┴───────────────┴────────────┴────────────┴─────────────┴──────────┘
```

# What had been tested?

TODO

# Understading the results

TODO
