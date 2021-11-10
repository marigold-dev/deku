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

#### Tmuxinator (optional)

Tmuxinator manages running multiple commands in a terminal session. We
provide [config file](./.tmuxinator.yml) to help you get started with
it. 

### Testing

We use [Rely](https://reason-native.com/docs/rely/) to write
tests. These can be run with,

```
esy test
```

### Running a sidechain

For convenient local development, we have a included a script `sandbox.sh` that
automates the setup of a local Tezos network using [Flextesa](https://tezos.gitlab.io/flextesa/)
running in Docker. Additionally, the script sets up a identities for a local Deku cluster.
This script is the easiest way to get started with Deku; however, it uses unsafe
configuration options to lower the required Tezos confirmations to 1. This setting greatly
speeds up local development, **but is not at all safe for production**!

#### Requirements
- The sandbox requires [Docker](https://docs.docker.com/get-docker/) to be installed.
- Install the Tezos client. The easiest way to do this is by installing the static Tezos binaries
  if your system package manager supports it. See the [Tezos documentation](https://tezos.gitlab.io/introduction/howtoget.html#getting-static-binaries) for details:
    ```
    # On Ubuntu
    sudo add-apt-repository ppa:serokell/tezos && sudo apt-get update
    sudo apt-get install -y tezos-client
    # On Mac
    brew tap serokell/tezos-packaging https://github.com/serokell/tezos-packaging.git
    brew install tezos-client
    ```

    Additionally, there are [instructions for building from source](https://tezos.gitlab.io/introduction/howtoget.html#building-from-sources-via-opam)
    using OPAM.

- Install the Ligo compiler toolchain. There are [instructions](https://ligolang.org/docs/intro/installation) on the Ligo website.
  Additionally, there is a [Nix](https://nixos.org/) package for Ligo
    ```
    nix-env --install ligo
    ```

#### Setup

Run `./sandbox.sh start` to start a local Tezos sandbox network, setup three Deku validator node identities, and deploy
a Deku consensus contract configured for these validators to the local sandbox.

Run `./sandbox.sh stop` to kill the Tezos sandbox network.

#### Start


If you have tmuxinator installed, simply run:
```
tmuxinator
```

This will open 3 shells for the node and one shell to inject the genesis block. You can stop the session by pressing `CTRL+B`
and typing `:kill-session`.

Alternatively, to run the nodes as background processes without tmuxinator, simply run
```
./start.sh
```

You may have to stop the session with `killall deku-node` to fully shutdown the cluster.

In either case, if all goes well, you should see the block height displayed in the terminal and increasing every second or so.

## Contributing

Please refer the developer wiki [here](https://github.com/marigold-dev/sidechain/wiki)
