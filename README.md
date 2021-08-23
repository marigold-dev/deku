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

### Steps

1. Clone the repo
2. Run `esy`
3. Run `esy tmuxinator`

### Testing

We use [Rely](https://reason-native.com/docs/rely/) to write
tests. These can be run with,

```
esy test
```

### Running a sidechain

#### Requirements

To run a sidechain you will need to have a Tezos node that you control and a Tezos Secret Key(edsk).

You can run a Tezos node on a testnet and use the faucet to acquire a key with funds, as described in the following link:

- https://tezos.b9lab.com/tezos-basics/testnet-1

To find out your `edsk` you can run

```shell
./tezos-client show address <ADDRESS-NAME> -S
```

#### Setup

To run chain you will need to run a couple of commands, as those are currently unstable, this repo provide a guided script to setup.

```shell
./setup.sh
```

You will be prompted to deploy a smart contract and to inform the KT1 address to the deployed contract, the URL to your Tezos Node and your `edsk` key.

#### Start

As starting a local chain means running multiple commands locally we also provide a script to do it.

```shell
./start.sh
```

It will start all your nodes, produce a block and sign it, this will start the chain, if everything did go well you should be seeing the block height being displayed on the command line.

## Contributing

Please refer the developer wiki [here](https://github.com/marigold-dev/sidechain/wiki)
