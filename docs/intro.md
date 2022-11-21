---
sidebar_position: 1
---

# Introduction

## Raison d'être

Deku is a framework for developing high-performance, application specific [sidechains](https://www.marigold.dev/post/announcing-deku-c-betanet) that interop with the Tezos blockchain.

Deku blockchains use a fast, deterministic consensus algorithm based on Tendermint to achieve
consensus on blocks. Deku is parametric with respect to the virtual machine being run,
allowing developers to write custom virtual machine tailored to their application. SDKs for the VM exist
in NodeJS and OCaml, but the [VM protocol](https://github.com/marigold-dev/deku/blob/main/src/external_vm/external_vm_server.ml) can be implemented in any language.

Deku has native support for Tezos tickets, allowing virtually any type of asset to be transferred
to and from Deku networks via the [bridge contract](https://github.com/marigold-dev/deku/blob/main/src/tezos_interop/consensus.mligo) in the timespan of a single Tezos block.

Deku is used to power Deku Canonical - an L2 WebAssembly smart contract platform for Tezos, operated by Marigold.

## Getting Started

To develop and deploy a WASM smart contract to Deku-C, head over to the [Tuna compiler docs](https://github.com/marigold-dev/tuna).

To develop DApps and client-side applications for Deku networks, see [the client library](https://github.com/marigold-dev/deku/tree/main/client) and the [the OpenAPI schema](https://github.com/marigold-dev/deku/blob/main/docs/api.json). You can fork the [Deku-C DApp template](https://github.com/marigold-dev/deku-c-dapp-template) to get started developing DApps Deku-C.

<!-- TODO: fix the template
  To get started developing your own Deku network, you can fork the [Deku template](https://github.com/marigold-dev/deku-template). -->
