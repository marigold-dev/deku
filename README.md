# Marigold Sidechain

A sidechain to Tezos by Marigold focussed on higher throughput.

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

### Running the node

The node binary is `http_server.exe` present in `bin/` folder. One way
to execute it would be,

```
esy b dune exec ./bin/http_server.exe <args>
```

(`http_server` because the node uses HTTP for p2p communication)

However, every node needs an identity before it can join or start a
network. Identities can be created with the CLI tool, `sidecli`

```
esy x sidecli make-credentials
```

`make-credentials` meant for quick development - it creates a couple
of identities to setup a local test network. They're store in folders
`0`, `1`, `2` and `3`.

This identity folder can now be passed to the node.

```
esy b dune exec ./bin/http_server.exe 0
```

#### Running multiple nodes
Note that, due to a bug in esy, it isn't currently possible to run two
`esy b dune exec ./bin/http_server.exe ...` simultaneously. To
workaround this, use the full path, after entering `esy shell`, to the binary for now.

```
esy shell
$cur__target_dir/default/bin/http_server.exe ...
```

## Contributing

### Guidelines

Please consider the following,

1. Using `<your username>/<branchname>` format for the branches
2. Opening follow up posts on the issue tracker for non-critical
   issues observed during reviews of critical PRs.
   
### Conventions

See [Hacking](./HACKING.md)

### Resources

1. [Whimsical Diagrams](https://whimsical.com/sidechain-Hn48PizK75qk4weaU1GuVA)
2. [Notes](./notes)
