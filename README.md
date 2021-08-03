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

#### Deployment

`esy x http_server` would run the node (currently over HTTP). `http_server` takes one argument - folder containing the config and data files.

Example usage,

```sh
esy x http_server data/0
```

##### TLDR; If you can follow annotated shell scripts, checkout [setup.sh](./setup.sh), which contains the following steps as sequence of sh commands. You'll need [jq](https://github.com/stedolan/jq)

To configure and setup the node, 
1. Create identity for the node with `esy x sidecli generate-identity`. 
   Step #1 will most likely be run multiple times for each node run by each participant. We create a list of identities of such authorised participants (validators), and use them in the next step - originating the consensus contract.
2. `consensus.mligo` (found in `tezos_interop/`) needs to be deployed - a preloaded LIGO IDE with the contract can be found [here](https://ide.ligolang.org/p/-x6CdYJ5tIEaVzD9lGYsaA). Specify the list of validators' public addresses in `current_validators` field of the storage. Example,

```ml
{
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = ([("edpk...": key)]: validators);
  };
  vault = {
    known_handles_hash = (Big_map.empty : vault_known_handles_hash_set);
    used_handles = (Big_map.empty : vault_used_handle_set);
    vault = (Big_map.empty : vault);
  }
}
```
3. Use `setup-node` subcommand from `sidecli` to generate the config and the data files

```sh
    esy x sidecli setup-node "$data_directory" \
	--secret=< \
	--tezos_consensus_contract $contract_address_from_step_2 \
	--tezos_rpc_node https://testnet-tezos.giganode.io \
	--tezos_secret $test_net_wallet_secret \
	--uri=$uri
```

**Obtaining the `tezos_secret` key
This option takes the private key of the wallet to (TODO: do what?). While working with the test network, one needs to import the `secret` from the activate code JSON downloaded from [faucet.tzalpha.net](https://faucet.tzalpha.net) and activate the address.

```sh
# To import the keys obtained from faucet
tezos-client -p "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i" --endpoint https://testnet-tezos.giganode.io import keys from mnemonic <any alias here>

# To activate the address
tezos-client -p "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i" --endpoint https://testnet-tezos.giganode.io activate account alice with "json-file-downloaded-from-faucet.json"
```

The secret key can be found in `~/.tezos-client/secret_keys`

4. Run the node

```sh
esy x http_server $data_directory
```

There's a `.tmuxinator.yml` that can be used to open pre-configured tmux screen with multiple instances of the node running, for convenience.

##### Testing the node - producing a block

  To see the chain in action, blocks can be produced by hand with the following steps
  
  1. Produce a block and note the block hash
  
  ```sh
  HASH=$(esy x sidecli produce-block ./0/wallet.json ./0/state.bin | awk '{ print $2 }')
  ```
  
  2. Sign the block at each validator
  
  ```sh
  esy x sidecli sign-block 0/wallet.json $HASH
  esy x sidecli sign-block 1/wallet.json $HASH
  esy x sidecli sign-block 2/wallet.json $HASH
  ```

#### For Development
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

### Creating files needed to run the node

4 files are needed to set up the node. Creating them manually is inconvenient and error-prone, so sidecli has a convenience subcommand `setup-node` that takes all the information needed to create a node and creates the necessary files in a folder you specify.

Example usage:

```
esy x sidecli setup-node ./data/ \
   --secret edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd \
   --uri self_uri \
   --tezos_rpc_node tezos_node_uri \
   --tezos_secret edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd \
   --tezos_consensus_contract KT1DMCGGiHT2dgjjXHG7qh1C1maFchrLNphx
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
3. Specify the problem description along the with proposed solution 
   in the PR description. Refer merged PRs to get an idea of what is 
   considered a good PR description.
4. Specify if a PR depends on another PR before it can be merged.
   
### Conventions

See [Hacking](./HACKING.md)

### Resources

1. [Whimsical Diagrams](https://whimsical.com/sidechain-Hn48PizK75qk4weaU1GuVA)
2. [Notes](./notes)
