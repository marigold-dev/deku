# Build state root hash

## Tezos operations and user operations

1. Base case 

- Tezos operations: 2 deposits
- User operations: contract origination, contract innovation, transfer, withdraw.

```
Estimated testing time 10s (1 benchmarks x 10s). Change using '-quota'.
┌──────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
│ Name             │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
├──────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
│ bench state hash │ 257.36us │ 23.06kw │   3.68kw │  656.17w │    100.00% │
└──────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘
```
