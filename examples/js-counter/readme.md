# Example: A Counter VM in Typescript

`example.js` accepts increment or decrement operation.
It reads a value from the Deku store, and then writes a
new value in its place.

## Test the vm without a running a deku-cluster 

You can then run the example against a test JSON database using
the Deku CLI. For example:
```bash
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --' # If you don't have nix, deku-cli can be found in the _build folder of the project
action='{"Action": "Increment"}'
db='[["counter", 41]]'
echo "$db" | deku-cli create-mock-transaction ./wallet.json $action ./vm
```

## Test the vm with a running deku-cluster

```bash
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --' # If you don't have nix, deku-cli can be found in the _build folder of the project
action='{"Action": "Increment"}'
deku-cli create-custom-transaction ./wallet.json $action ./vm
```
It will return an operation hash. You can trace the operation in the log the vm.


## Notes

You can generate a wallet with:
```bash
deku-cli create-wallet
```
