# Marigold Sidechain

A sidechain to Tezos by Marigold focused on higher throughput.

## Getting started

### Pre-requisites

#### Esy Package Manager

Marigold Sidechain is being developed with
[esy](https://esy.sh/). `esy` can be installed with NPM or Yarn.

```sh
npm i -g esy # on Ubuntu you might need --unsafe-perms to work around EACCES issues
# or
yarn global add esy
```

Alternatively, you can use the `nix` package manager with
[flakes enabled](https://nixos.wiki/wiki/Flakes#Installing_flakes) to enter an
environment preloaded with the correct dependencies, including `esy`:

```
nix --experimental-features "nix-command flakes" run
```

(We suggest using this [binary cache repository](https://app.cachix.org/cache/anmonteiro) to reduce
build times)

### Testing

We use [Rely](https://reason-native.com/docs/rely/) to write
tests. These can be run with,

```
esy test
```

### Running a sidechain

For convenient local development, we have included two components:

- A `docker-compose.yml` file that setups a local Tezos network
  using [Flextesa](https://tezos.gitlab.io/flextesa/) running in Docker.
  Additionally, the network sets up a [Better Call Dev](https://github.com/baking-bad/bcdhub) instance
  for introspection of the deployed contract and its operations. The BCD interace is available in
  your browser at http://localhost:8000.
- A script `./sandbox.sh` that sets up a identities for a local Deku cluster.
  This script is the easiest way to get started with Deku; however, it uses unsafe
  configuration options to lower the required Tezos confirmations to 1. This setting greatly
  speeds up local development, **but is not at all safe for production**!

#### Requirements

The sandbox requires Bash, [Docker](https://docs.docker.com/get-docker/), and docker-compose to be installed,
in addition to the usual Deku pre-requisites.

#### Setup

Run `docker-compose up -d` to start a Tezos network and the BCD interface.

Run `./sandbox.sh setup` to start a local Tezos sandbox network, setup three Deku validator node identities, and deploy
a Deku consensus contract configured for these validators to the local sandbox.

Run `./sandbox.sh tear-down` to kill the Tezos sandbox network and wipe the Deku state.

Run `docker-compose down -v` to stop the Tezos network and destroy the BCD database.

#### Start

Simply run:

```
./sandbox.sh start
```

This starts 3 nodes as background processes.

You may have to stop the session with `killall deku-node` to fully shutdown the cluster.

In either case, if all goes well, you should see the block height displayed in the terminal and increasing every second or so.

## Contributing

Please refer the developer wiki [here](https://github.com/marigold-dev/sidechain/wiki)
