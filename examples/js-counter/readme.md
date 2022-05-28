# Example: A Counter VM in Typescript

In `example.ts` that accepts increment or decrement transactions,
reads a value from the Deku store, and then writes a
new value in its place.

You can install the example with (don't forget to build the sdk too):
```bash
npm i
```

You can then run the example against a test JSON database using
the Deku CLI. For example:
```bash
alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --'
action='{"Action": "Increment"}'
deku-cli create-mock-transaction ./wallet.json $action node example.js
```

# Notes:

The initial state is provided by the vm
