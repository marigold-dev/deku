# Marigold Sidechain

A sidechain to Tezos by Marigold focused on higher throughput.

## Getting started

### Pre-requisites

Deku supports the following platforms: x86_64-linux, aarch64-linux, and
aarch64-darwin (Mac M1).

#### Nix

Install Nix by following the [official guide](https://nixos.org/download.html).
Additionally, you must enable the [flakes feature](https://nixos.wiki/wiki/Flakes):
```shell
echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf
```

Nix automatically installs all the dependencies and devtools needed for development,
including OCaml, Docker, etc.

You can load the Nix development with the command:
```shell
cd deku
nix develop -c $SHELL
```

We recommend installing [`direnv`](https://direnv.net/) to automatically load the
Nix environment for you whenever you enter the directory. Once installed, you can
enable `direnv` for Deku with:
```shell
cd deku
direnv allow
```

### Building

```shell
dune build
```

### Testing

Run our unit tests with:
```shell
dune build @runtest
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

The sandbox requires Bash and [Docker](https://docs.docker.com/get-docker/) installed.
in addition to the usual Deku pre-requisites (alternatively, you can use `nix` to provide these).

#### Setup

Run `docker compose up -d` to start a Tezos network and the BCD interface.

Run `./sandbox.sh setup` to start a local Tezos sandbox network, setup three Deku validator node identities, and deploy
a Deku consensus contract configured for these validators to the local sandbox.

Run `./sandbox.sh tear-down` to kill the Tezos sandbox network and wipe the Deku state.

Run `docker compose down -v` to stop the Tezos network and destroy the BCD database.

#### Start

Simply run:

```shell
./sandbox.sh start
```

This starts 3 nodes as background processes.

You may have to stop the session with `killall deku-node` to fully shutdown the cluster.

In either case, if all goes well, you should see the block height displayed in the terminal and increasing every second or so.

#### Experimental: Tilt

Our experimental workflow uses [Tilt](https://tilt.dev) which is an advanced orchestrator. If you're using `nix` everything is already provided. If not, you will have to install the things in [requirements](#requirements) + [tilt](https://docs.tilt.dev/install.html) and [docker-compose](https://docs.docker.com/compose/install/).

When the requirements are in place you should just have to run `tilt up` and everything will be running.

## Contributing

Please refer the developer wiki [here](https://github.com/marigold-dev/sidechain/wiki)
