# Marigold Sidechain

A sidechain to Tezos by Marigold focused on higher throughput.

## Getting started

### Pre-requisites

Marigold Sidechain is being developed with the
[esy](https://esy.sh/) package manager. `esy` can be installed with NPM or Yarn.

```sh
npm i -g esy # on Ubuntu you might need --unsafe-perms to work around EACCES issues
# or
yarn global add esy
```

Alternatively, you can use the `nix` package manager with
[flakes enabled](https://nixos.wiki/wiki/Flakes#Installing_flakes) to enter an environment
preloaded with the correct dependencies and devtools.

1. With [`direnv`](https://direnv.net/) (recommended):

```shell script
$ which docker
docker not found
$ cd deku
$ direnv allow
direnv: loading ~/workspace/marigold/deku/.envrc
# [...]
$ which docker
/nix/store/aagbv31cpinw832vyhh9gscb29gi363c-docker-20.10.12/bin/docker
```

2. With `nix-command`:

```
nix --experimental-features "nix-command flakes" develop -c $SHELL
```

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
in addition to the usual Deku pre-requisites (alternatively, you can use `nix` to provide these).

#### Setup

Run `docker-compose up -d` to start a Tezos network and the BCD interface.

Run `./sandbox.sh setup` to start a local Tezos sandbox network, setup three Deku validator node identities, and deploy
a Deku consensus contract configured for these validators to the local sandbox.

Run `./sandbox.sh tear-down` to kill the Tezos sandbox network and wipe the Deku state.

Run `docker-compose down -v` to stop the Tezos network and destroy the BCD database.

#### Start

Simply run:

```
./start.sh
```

This starts 3 nodes as background processes.

You may have to stop the session with `killall deku-node` to fully shutdown the cluster.

In either case, if all goes well, you should see the block height displayed in the terminal and increasing every second or so.

## Contributing

Please refer the developer wiki [here](https://github.com/marigold-dev/sidechain/wiki)
