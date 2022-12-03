<p align="center">
    <img alt="Deku" src="https://uploads-ssl.webflow.com/616ab4741d375d1642c19027/62c3e74e6c26549d5e660805_deku-p-500.png" width="200" />
    <div align="center">A Sidechain Framework for Tezos</div>
</p>

## Raison d'être

Deku is a framework for developing high-performance, application specific [sidechains](https://www.marigold.dev/post/announcing-deku-c-betanet) that interop with the Tezos blockchain.

Deku blockchains use a fast, deterministic consensus algorithm based on Tendermint to acheive
consensus on blocks. Deku is parametric with respect to the virtual machine being run,
allowing developers to write custom virtual machine tailored to their application. SDK's for the VM exist
in NodeJS and OCaml, but the [VM protocol](https://github.com/marigold-dev/deku/blob/main/src/external_vm/external_vm_server.ml) can be implemented in any language.

Deku has native support for Tezos tickets, allowing virtually any type of asset to be transferred
to and from Deku networks via the [bridge contract](https://github.com/marigold-dev/deku/blob/main/src/tezos_interop/consensus.mligo) in the timespan of a single Tezos block.

Deku is used to power Deku Canonical - an L2 WebAssembly smart contract platform for Tezos operated by Marigold.

## Getting Started

To develop and deploy a WASM smart contract to Deku-C, head over to the [Tuna compiler docs](https://github.com/marigold-dev/tuna).

To develop DApps and client-side applications for Deku networks, see [the client library](https://github.com/marigold-dev/deku/tree/main/client) and the [the OpenAPI schema](https://github.com/marigold-dev/deku/blob/main/docs/api.json). You can fork the [Deku-C DApp template](https://github.com/marigold-dev/deku-c-dapp-template) to get started developing DApps Deku-C.

To get started developing your own Deku network, you can fork the [Deku template](https://github.com/marigold-dev/deku-template).

## Development

### Building from Source

We currently support building from source on Linux x86_64 and MacOS M1 systems.

Deku is packaged with Nix. See the [Nix docs](https://nixos.org/download.html) for instructions for your system.
Additionally, ensure [Nix flakes are enabled](https://nixos.wiki/wiki/Flakes#Enable_flakes).

Once Nix is installed, the dev environment can build built with:

```
nix develop .
```

### Tests and Benchmarks

Tests and benchmarks can be run with the respective commands:

```
nix develop -c dune build @runtest
nix run .#benchmark
```

### Running the development network

Our sandbox network uses [Flextesa](https://tezos.gitlab.io/flextesa/) to run a local Tezos network
via `docker compose`. Additionally, our compose file includes a local instance of https://better-call.dev
that can be used to inspect and interact with any contracts you deploy to the Tezos sandbox network.

First, start the Flextesa sandbox. You'll need to login it the Github container registery
which you can do by following [these instructions](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry).

```
docker compose up -d
```

Deku chains are CPU-intensive, so depending on your available resources you may
want to lower the default block size (default is 10000 operations/block):

```
export DEKU_DEFAULT_BLOCK_SIZE=100
```

You can start the chain with:

```
./start.sh
```

This will run nodes listening for gossip on ports 4440, 4441, 4442, and 4443 respectively.
Node 0 will also run a user-facing API on port 8080. E.g.:

```
curl http://localhost:8080/api/v1/chain/blocks/head
```
