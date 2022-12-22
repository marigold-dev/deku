# Deku e2e tester

This tester can test some aspect of the deku-c blockchain:

- invokation
- origination
- are node sync ?
- are node making progress ?
- ticket transfer
- ticket deposit
- ticket withdraw
- is deku updating the state root hash of the consensus contract ?

I don't recommend to run the ticket tests in parralel, except if you use different secret in every tests.

# Requirement:

- node18 for execution

# How to run ?

If you are working in the deku monorepo, you will have to the following commands:

```
yarn install # At root position
cd deku-c/client
yarn build
cd ../../examples/tests
yarn build
```

Then you can run the help of the tests to check everything.

```
node dist/index.js --help
```

If you want some precise help on a command:

```
node dist/index.js <command> --help
```
