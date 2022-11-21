---
sidebar_position: 3
---

# Deku-C CLI

We provide a small CLI client in TypeScript to access Deku-C. This client allows you to originate
new smart contracts, inspect their state or send transactions to call their entrypoints.

To use the client, first install it with `npm install @marigold-dev/deku-cli`.

## Basic usage

### Generating an identity

In order to originate contracts or post transactions, you first need a wallet with an address and
the associated private key. You can either take an existing Ed25519 private key (associated with a
tz1 address) or generate a new one with the following command:

```bash
node index.js generate-identity -o your-wallet.json
```

This will create a new file called `your-wallet.json`, containing something like

```js
{
  "address": "tz1Vhy8BWXdQNm6wNFi36hWj4iSoGqBrMB4v",
  "priv_key": "edsk4J5PQ8UZ2HrtvnDeRuL8uA1y8isPBrRDkPRkPvTn6SQYMGJ3WV"
}
```

### Originating a contract

Deku-C uses the WASM VM, for which you can target with a variety of languages. In particular, you can
use contracts written or compiled to the Michelson language, which are compiled on-the-fly towards
WASM using the `originate` command. Usage:

```bash
node index.js originate wallet.json contract.tz initial_expression
```

where
- `wallet.json` is the path to a wallet file;
- `contract.tz` is the  path to a contract file, in Michelson;
- `initial_expression` is the initial state of the storage at origination, in Michelson.

The command provides the hash of the origination operation as well as the address of the contract.
For instance:

```bash
$ node index.js originate wallet.json contract.tz 'Pair 0 None'

operation hash: Do2T2AEN5YheH8YpNgx1ysaU1XP8MmWborfFk5EkszXBCk67fzWA
Contract originated at address DK1CeFk3tNkRNvCM2hSwCxV5kCUEy2gKzC73
```

:::caution

The contract address is printed even if the origination fails. Unlike the `tezos-client` tool,
`deku-cli` does not check that the origination was successful, nor does it wait for the operation's inclusion.
:::

Alternatively, you can deploy a smart contract written in Ligo directly. To compile the contract,
the CLI will call another API hosted by Marigold and upload your contract, then call the Deku-C with
the resulting Michelson code. For the moment, the initial storage expression still has to be written
in Michelson, and the contract is limited to one file.

### Inspecting a smart contract

The CLI provides two commands to inspect a smart contract from its address. First, you can list its entrypoints
using the `show-entrypoints` command. By default, they are listed by their name; however, to call
those entrypoints (which we show in the next section) it can be easier to print the corresponding
Michelson expression as well. For this, use the `--verbose` flag:

```bash
$ node index.js show-entrypoints --verbose DK1Goyabut31X3kbZNW1i4qHKoX9sdMF8JhL
{
  '%decrement': [ 'Left', 'Left' ],
  '%increment': [ 'Left', 'Right' ],
  '%reset': [ 'Right' ]
}
```

The storage of a smart contract can be inspected with the `show-storage` command. This will print a
JSON expression representing the current state of the contract.

:::caution

The printed storage expression show the internal state of the VM for the contract. In general, it is
not valid Michelson or Ligo code.
:::

More information can be printed using the `show-storage --raw` option, including the originator of
the contract and the raw code of the contract.

### Calling a smart contract

The `invoke` command allows to call an entrypoint with a specific parameter. Usage:

```bash
node index.js wallet contract_address parameter
```

where
- `wallet` is a path towards a wallet file
- `contract_address` is the address of the smart contract on the Deku-C chain
- `parameter` is the Michelson expression of the entrypoint and its argument

The `parameter` can be provided by the Ligo compiler using the `ligo compile parameter` command.

## Example: deploying and calling a smart contract

TBD
