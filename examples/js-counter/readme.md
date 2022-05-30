# Example: A Counter VM in Typescript

`example.ts` accepts increment or decrement operations,
reads a value from the Deku state, and then writes a
new value in its place.

# Run the example without a deku cluster

You can then run the example against a test JSON database using
the Deku CLI. For example:
```bash
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --'
action='{"Action": "Increment"}'
db='[["counter", 41]]'
deku-cli create-mock-transaction ./wallet.json $action "node example.js" 
```

# Run the example with a deku cluster

Runs a deku cluser following the readme of the project, it will by default run this vm.

Then you can interact with the vm via the deku client:
```bash
action='{"Action": "Increment"}'
deku-cli create-custom-transaction data/0 ./wallet.json $action # where data is the data/0 folder of a node
```

# Notes

You can create a wallet with
```bash
deku-cli create-wallet
```

If you don't have nix, you can find the binary of the deku client in the `_build` folder





