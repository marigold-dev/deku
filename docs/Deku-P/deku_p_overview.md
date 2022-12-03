---
sidebar_position: 2
---

# Deku-P Overview

Deku-P is a framework for building high-performance sidechains for Tezos.
By providing a binary that adheres to a simple protocol, developers
can design their own application-specific virtual machines in any programming language
and run them as Proof-of-Authority blockchains with full access to the Tezos ecosystem.

In this article, we'll give an overview of Deku-P's main concepts.

## What is a sidechain?

A sidechain is a blockchain with a [light client](https://ethereum.org/en/developers/docs/nodes-and-clients/#light-node)
embedded in a host blockchain. In the case of Deku-P sidechains, the light client is written as a smart contract deployed
to Tezos called the bridge contract. Each sidechain has its own bridge contract.
Deku-P validator nodes receive transactions from users, organize them into blocks, and execute them to determine a new state.
Additionally, they periodically post their block headers to the bridge contract, allowing Tezos to witness the
state of sidechain's consensus.

## Deku-P consensus

Deku-P sidechains use a Tendermint-based consensus to order blocks.
This algorithm is based on Proof-of-Authority - a group of trusted parties are selected ahead of time,
and only the blocks with a 2/3rds supermajority of signatures from these parties are accepted. The bridge
contract on Tezos acts as an oracle by which users can identify who the current validators are for a particular
sidechain at any given moment.

While the bridge contract mechanism works similarly to rollups, Deku-P sidechains require a greater trust
threshold (supermajority, as opposed to any-trust). However, unlike with rollups, a sidechain's
block production is decoupled from Tezos, unlocking much greater bandwidth and lower latency (you can read more
about this in [the architecture doc](./architecture.md)).

## Tezos Interop

Users can deposit Tezos assets in the form of [tickets](https://www.marigold.dev/post/tickets-for-dummies) to Deku-P chains
by locking them in the bridge contract's vault. Deposits are finalized and available on the sidechain in the span of a single
Tezos finality period (two Tezos blocks).

Users can withdraw assets from Deku-P chains to Tezos by burning them on the Deku side. Burn receipts are collected
into a [Merkle root hash](https://en.wikipedia.org/wiki/Merkle_tree) and included in the Deku block headers. When the block
is posted to the Tezos bridge contract, users can present a Merkle proof witnessing the inclusion of their receipt
and finalize their withdraw on Tezos.

In exchange for requiring less trust, withdraws from interactive rollups are bound to the
worst-case latency of the interactive refutation game and can take weeks to finalize. Withdraws from Deku-P sidechains
are finalized and available on Tezos in two Tezos blocks (about a minute).

Virtually any Tezos asset can be wrapped in a ticket and deposited to Deku. [TzPortal](https://ghostnet.tzportal.marigold.dev/)
provides a user-friendly interface automating the process of wrapping FA1.2 and FA2 tokens in tickets and depositing to/withdrawing from
Deku-P sidechains.

## Deku-P Virtual Machines

While Tezos is specialized to run the Michelson virtual machine and Ethereum specialized to run EVM, Deku-P is _parametric_
with respect to the virtual machine it runs. Deku nodes assemble user transactions into blocks and submit them to
the VM - transaction semantics are left entirely up to you.

Deku VM's communicate with the node over [named pipes](https://en.wikipedia.org/wiki/Named_pipe). When a transaction
is finalized, it is sent to the VM, which responds with a series of reads and writes to the underlying data store. The VM
can implement any business logic you desire, but it must be completely deterministic, else individual nodes could get out of
sync, compromising the safety of the network.

We have SDK's in Typescript, Go and OCaml, but supporting new languages is easy - see the [VM protocol docs](./vm_protocol.md)
for more.

## Summary

It's useful to compare/contrast Deku-P sidechains with Tezos itself and with rollups:

|                               | Tezos                 | Rollups                | Deku-P              |
| ----------------------------- | --------------------- | ---------------------- | ------------------- |
| Additional Trust Beyond Tezos | N/A                   | any-trust              | 2/3rd supermajority |
| Block Latency                 | 30s                   | 30s (coupled to Tezos) | 2s                  |
| Throughput                    | 200 tx/s              | 1000 tx/s              | 50,000 tx/s         |
| Possible to run in private?   | No                    | No                     | Yes                 |
| Language Support              | Ligo, SmartPy, et al. | same as Tezos + WASM   | Any language        |
| Deposit Latency               | N/A                   | 1 min                  | 1 min               |
| Withdraw Latency              | N/A                   | two weeks              | 1 min               |

Sidechains require additional trust but in exchange offer more flexibility, lower latency,
and higher bandwidth.
