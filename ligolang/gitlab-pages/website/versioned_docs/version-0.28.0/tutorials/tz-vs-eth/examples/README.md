# Migrating from Ethereum guide

These examples demonstrate features of LIGO and act as a supplementary material for the "Migrating from Ethereum" tutorial.

## Contracts

* SolidityLikeDispatching – a contract that demonstrates how we can emulate the Solidity compiler method dispatching.
* Counter – a counter contract that demonstrates the use of entrypoints in LIGO.
* Lambda – an example that demonstrates how we can pass lambdas as parameters.
* LambdaInStorage — a contract that shows how we can save lambdas to storage to make our contracts upgradeable.

## Running examples

1. Run `npx ganache-cli --flavor tezos --seed alice` in the background.
2. Use `truffle migrate` to originate the contracts and `npm run test` to run the tests.
