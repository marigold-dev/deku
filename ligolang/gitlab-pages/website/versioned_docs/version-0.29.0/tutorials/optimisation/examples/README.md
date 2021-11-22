# Optimisation examples

These examples act as a supplementary material for the optimisation tutorial. Here you can find four groups of contracts that showcase different optimisation techniques:

1. Constants optimisation (`contracts/constants`):
  * V1 – a non-optimised version that uses a large constant
  * V2 – an optimised version that uses a smaller constant
2. Basic function inlining (`contracts/inlining`):
  * V1 – a version with no inlining
  * V2 – a contract with one of the functions marked as "inline"
3. More controversial inlining (`contracts/effectful_binding`):
  * V1 – an attempt to inline a non-pure binding
  * V2 – a contract with the non-pure computation wrapped in an inline function
  * V3 – same as V2 but without inlining
4. Using lazy storage to store a large entrypoint (`contracts/large_entrypoint`):
  * V1 – a contract with two entrypoints – a large one and a small one
  * V2 – the same contract with the large entrypoint moved to a big map

You can use the provided script (`./bin/estimateGas`) to compare the different version.

## Running examples

1. Run `npx ganache-cli --flavor tezos --seed alice` in the background.
2. Use `npx truffle migrate` to originate the contracts.
3. Test the contracts' size and gas consumption:
   * Use `ligo info measure-contract <PATH> --entry-point main` to measure the contract size
   * Run the script `./bin/estimateGas` to see the gas and fees estimates for each of the contracts

## Switching between the dialects

By default, all the scripts run CameLIGO contracts. To change this behaviour, set the `syntax` variable in `truffle-config.js` to one of `ligo`, `mligo`, or `religo`. You would need to re-run your migrations to deploy the new contracts to your development network (run `npx truffle migrate --reset` to do it).
