---
id: operators
title: Operators
---

## Available Operators

> This list is non-exhaustive. More operators will be added in
> upcoming LIGO releases.

|Michelson   	|Pascaligo   	|Description |
|---	|---	|---	|
| `SENDER` | `Tezos.sender` | Address that initiated the current transaction
| `SOURCE` | `Tezos.source` | Address that initiated the transaction, which triggered the current transaction. (useful e.g. when there's a transaction sent by another contract)
| `AMOUNT` | `Tezos.amount` | Amount of tez sent by the transaction that invoked the contract
| `NOW`    | `Tezos.now`    | Timestamp of the block whose validation triggered execution of the contract, i.e. current time when the contract is run.
