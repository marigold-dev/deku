# Inter-contract invocations

These examples demonstrate how one contract can call – or, rather, _invoke,_ other contracts.

## Contracts

* SimpleCounter – a contract that adds the integer from the storage to the integer from the parameter, and saves the sum to the storage.
* AdvancedCounter – a contract that supports more arithmetic operations: it can add, subtract, and multiply the values, or reset the counter based on the entrypoint being called.
* Proxy — a simple proxy contract that calls the default entrypoint of the callee.
* EntrypointProxy – a proxy that calls the `%add` entrypoint of the callee.
* CreateAndCall – a contract that originates a new contract and calls it in one transaction.

## Running examples

1. Run `npx ganache-cli --flavor tezos --seed alice` in the background.
2. Use `truffle migrate` to originate the contracts and `npm run test` to run the tests.

Note that `npm run test` calls a custom script to run the tests: currently, Truffle for Tezos does not provide a "clean room" environment for testing, so running `truffle test` will likely fail because the tests for the contracts are not isolated.
